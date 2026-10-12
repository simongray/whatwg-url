(ns dk.simongray.whatwg-url
  "URLs as the URL Standard has browsers read, resolve, write and compare
  them, as plain maps.

  A URL is a map of the parts it has:

  - :scheme, in lower case, e.g. \"https\"
  - :username and :password, percent-encoded, when they aren't empty
  - :host, as its text, e.g. \"example.com\", \"127.0.0.1\" or \"[::1]\",
    which can be empty, e.g. in file:///etc/hosts
  - :port, a number, when it isn't the default of the scheme
  - :path, a vector of segments, or a string for an opaque path, e.g. that
    of mailto:me@example.com
  - :query and :fragment, without their ? and #, which can be empty

  The functions that take a URL take a map, or its text, e.g. a string or
  a java.net.URI, which they parse as an absolute URL first. They give nil
  for text that doesn't parse.

  The functions that are named after an attribute of the URL class of the
  standard, e.g. hostname and pathname, give its value, and with and update
  set them as its setters do."
  (:refer-clojure :exclude [hash resolve update])
  (:require [clojure.string :as str]
            [dk.simongray.whatwg-url.chars :as chars]
            [dk.simongray.whatwg-url.hosts :as hosts]
            [dk.simongray.whatwg-url.params :as params]
            [dk.simongray.whatwg-url.parser :as parser]
            [dk.simongray.whatwg-url.percent :as percent]))

(defn- base-url
  "The URL `base` to resolve against, parsed when it's text, nil when
  there's none, or ::invalid when it doesn't parse."
  [base]
  (cond
    (nil? base)  nil
    (map? base)  base
    :else        (or (:url (let [p (parser/run (str base) {})]
                             (when (= :done (:state p)) p)))
                     ::invalid)))

;; a map of at most eight keys keeps them in the order they're added
(defn- in-order
  "The URL map `url` with its parts in the order they're written in."
  [url]
  (persistent!
   (reduce (fn [m k] (if-some [v (get url k)] (assoc! m k v) m))
           (transient (array-map))
           [:scheme :username :password :host :port :path :query :fragment])))

;; URL Standard, section 6.1, the API URL parser
(defn parse
  "The URL that the text `s` writes, resolved against the URL `base` when
  given, or nil when it doesn't parse. The base is a map or text.

      (parse \"../c?q#f\" \"https://EXAMPLE.com/a/b\")
      ;; => {:scheme \"https\" :host \"example.com\" :path [\"c\"]
      ;;     :query \"q\" :fragment \"f\"}"
  ([s]
   (parse s nil))
  ([s base]
   (let [base (base-url base)]
     (when-not (= ::invalid base)
       (let [{:keys [state url]} (parser/run (str s) {:base base})]
         (when (= :done state)
           (in-order url)))))))

(defn- ->url
  "The `url`, parsed when it's text."
  [url]
  (cond
    (nil? url) nil
    (map? url) url
    :else      (parse url)))

(defn- host-code?
  "Whether the code `c` can be in a host that the parser leaves as it is,
  without IDNA: a lower-case letter, a digit, a dot or a hyphen."
  [^long c]
  (or (and (<= 0x61 c) (<= c 0x7A))
      (and (<= 0x30 c) (<= c 0x39))
      (== c 0x2E)
      (== c 0x2D)))

(defn- path-code?
  "Whether the code `c` can be in the path, the query or the fragment of an
  http or https URL that the parser leaves as it is, after the code
  `before`, with no dot segment."
  [^long before ^long c]
  (and (<= 0x21 c)
       (<= c 0x7E)
       (case (int c)
         (0x22 0x27 0x3C 0x3E 0x5C 0x5E 0x60 0x7B 0x7D) false
         true)
       (not (and (== 0x2F before) (or (== 0x2E c) (== 0x25 c))))))

;; most links of a page are written as they're serialized, which one pass
;; over them tells
(defn- normal?
  "Whether the text `s` is an http or https URL that the parser and the
  serializer give back as it is: with a host of lower-case letters,
  digits, dots and hyphens that doesn't end in a number, a path, and no
  port or credentials."
  [s]
  (let [n     (count s)
        start (cond
                (str/starts-with? s "https://") 8
                (str/starts-with? s "http://")  7)
        slash (when start
                (loop [i start]
                  (when (< i n)
                    (let [c (chars/code-at s i)]
                      (cond
                        (host-code? c) (recur (inc i))
                        (== 0x2F c)    i)))))]
    (boolean
     (and slash
          (< start slash)
          (not (hosts/ends-in-a-number? (subs s start slash)))
          (loop [i (inc slash) before 0x2F]
            (or (== i n)
                (let [c (chars/code-at s i)]
                  (and (path-code? before c) (recur (inc i) c)))))))))

;; URL Standard, section 4.5, the URL path serializer
(defn pathname
  "The path of the `url` as text, e.g. /a/b, as the pathname of the URL
  class gives it."
  [url]
  (when-let [{:keys [path]} (->url url)]
    (if (string? path)
      path
      (apply str (map #(str "/" %) path)))))

;; URL Standard, section 4.5, the URL serializer
(defn- serialize
  "The text of the URL map `url`, without its fragment unless `fragment?`."
  [url fragment?]
  (when-let [{:keys [scheme username password host port path query fragment]} url]
    (str scheme ":"
         (when host
           (str "//"
                (when (or username password)
                  (str username (when password (str ":" password)) "@"))
                host
                (when port (str ":" port))))
         ;; a path that starts with // would read as a host
         (when (and (nil? host) (vector? path) (< 1 (count path)) (= "" (first path)))
           "/.")
         (pathname url)
         (when query (str "?" query))
         (when (and fragment? fragment) (str "#" fragment)))))

(defn href
  "The text of the `url`, with `opts`:

  - :fragment?, false to leave out the fragment

      (href {:scheme \"https\" :host \"example.com\" :path [\"\"]})
      ;; => \"https://example.com/\""
  ([url]
   (href url {}))
  ([url {:keys [fragment?] :or {fragment? true}}]
   (if (and (string? url) (normal? url))
     (cond-> url
       (and (not fragment?) (str/includes? url "#")) (subs 0 (str/index-of url "#")))
     (serialize (->url url) fragment?))))

(defn resolve
  "The text of the URL `s` resolved against the URL `base`, as a browser
  resolves the link of a page against the page's URL, or nil when it
  doesn't parse. With a nil base, only an absolute URL parses.

      (resolve \"https://example.com/blog/post\" \"../about\")
      ;; => \"https://example.com/about\"

  For many URLs against the same base, use resolver, which parses the base
  only once."
  [base s]
  (let [base (base-url base)]
    (cond
      (= ::invalid base) nil
      (nil? s)           nil
      (normal? (str s))  (str s)
      :else              (some-> (parse s base) (serialize true)))))

(defn resolver
  "A function of a URL that resolves it against the URL `base` as resolve
  does, with the base parsed only once, e.g. for the links of a page.

      (map (resolver \"https://example.com/blog/post\") [\"a\" \"/b\"])
      ;; => (\"https://example.com/blog/a\" \"https://example.com/b\")"
  [base]
  (let [base (base-url base)]
    (fn [s]
      (when-not (= ::invalid base)
        (resolve base s)))))

(defn- report
  "The parse of the text `s` against the URL `base` with its validation
  errors, or nil when the base doesn't parse."
  [s base]
  (let [base (base-url base)]
    (when-not (= ::invalid base)
      (parser/run (str s) {:base base :errors? true}))))

(defn validation-errors
  "The validation errors of the text `s` as a URL resolved against `base`,
  as a vector of keywords in the order they're found, e.g.
  :invalid-credentials, or nil when the base doesn't parse. The errors are
  those that section 1.1 of the URL Standard names, in lower case. When `s`
  doesn't parse, the last error is the one that made it fail, if any.

      (validation-errors \"https://user@example.org/a b\")
      ;; => [:invalid-credentials :invalid-url-unit]"
  ([s]
   (validation-errors s nil))
  ([s base]
   (some-> (report s base) :errors distinct vec)))

(defn valid?
  "Whether the text `s` is a valid URL string, resolved against `base`:
  one that parses with no validation error."
  ([s]
   (valid? s nil))
  ([s base]
   (let [{:keys [state errors]} (report s base)]
     (and (= :done state) (empty? errors)))))

(defn special?
  "Whether the `url` has a special scheme: ftp, file, http, https, ws or
  wss."
  [url]
  (some-> (->url url) :scheme parser/special?))

;; URL Standard, section 4.7
(defn- origin-tuple
  "The scheme, host and port of the origin of the `url`, or nil when the
  origin is opaque."
  [{:keys [scheme host port] :as url}]
  (case scheme
    ("ftp" "http" "https" "ws" "wss") [scheme host port]
    "blob"                            (let [inner (parse (pathname url))]
                                        (when (#{"http" "https"} (:scheme inner))
                                          (origin-tuple inner)))
    nil))

(defn origin
  "The origin of the `url` as text, e.g. https://example.com:8443, or
  \"null\" when it's opaque, e.g. for a file URL.

  The origin of a blob URL is that of the URL in its path."
  [url]
  (when-let [url (->url url)]
    (if-let [[scheme host port] (origin-tuple url)]
      (str scheme "://" host (when port (str ":" port)))
      "null")))

(defn same-origin?
  "Whether the URLs `a` and `b` have the same scheme, host and port. An
  opaque origin, e.g. of a file URL, is the same as no other."
  [a b]
  (let [a (some-> (->url a) origin-tuple)]
    (boolean (and a (= a (some-> (->url b) origin-tuple))))))

;; URL Standard, section 4.6
(defn equivalent?
  "Whether the URLs `a` and `b` have the same text, with `opts`:

  - :fragment?, false to compare them without their fragments"
  ([a b]
   (equivalent? a b {}))
  ([a b opts]
   (let [a (href a opts)]
     (boolean (and a (= a (href b opts)))))))

(defn path-starts-with?
  "Whether the path of the `url` starts with the segments of the path of
  `prefix`, e.g. /a/b with /a but not /ab with /a, as cookies compare
  paths. An opaque path starts with nothing."
  [url prefix]
  (let [a (:path (->url url))
        b (:path (->url prefix))]
    (boolean (and (vector? a) (vector? b)
                  (<= (count b) (count a))
                  (= b (subvec a 0 (count b)))))))

(defn scheme
  "The scheme of the `url`, in lower case, e.g. https."
  [url]
  (:scheme (->url url)))

(defn protocol
  "The scheme of the `url` with a colon, e.g. https:."
  [url]
  (some-> (scheme url) (str ":")))

(defn username
  "The username of the `url`, percent-encoded, or an empty string."
  [url]
  (some-> (->url url) (:username "")))

(defn password
  "The password of the `url`, percent-encoded, or an empty string."
  [url]
  (some-> (->url url) (:password "")))

(defn hostname
  "The host of the `url`, or an empty string, e.g. example.com."
  [url]
  (some-> (->url url) (:host "")))

(defn port
  "The port of the `url` as text, or an empty string when it has none, as
  when it's the default of the scheme."
  [url]
  (some-> (->url url) (:port "") str))

(defn host
  "The host and port of the `url`, e.g. example.com:8443, or an empty
  string when it has no host."
  [url]
  (when-let [{:keys [host port]} (->url url)]
    (str host (when (and host port) (str ":" port)))))

(defn search
  "The query of the `url` with its ?, or an empty string when the query is
  empty or there's none."
  [url]
  (when-let [{:keys [query]} (->url url)]
    (if (seq query) (str "?" query) "")))

(defn hash
  "The fragment of the `url` with its #, or an empty string when the
  fragment is empty or there's none."
  [url]
  (when-let [{:keys [fragment]} (->url url)]
    (if (seq fragment) (str "#" fragment) "")))

(defn search-params
  "The name-value pairs of the query of the `url`, as the functions of
  dk.simongray.whatwg-url.params take them."
  [url]
  (some-> (->url url) (:query "") params/parse))

(defn- with-parse
  "The `url` as the basic URL parser changes it for a setter that starts
  in the `state` with the text `s`."
  [url state s]
  (:url (parser/run s {:url url :override state})))

(defn- no-credentials?
  "Whether the `url` cannot have a username, password or port."
  [{:keys [scheme host]}]
  (or (nil? host) (= "" host) (= "file" scheme)))

(defn- with-userinfo
  "The `url` with its username or password `k` set to the text `v`,
  percent-encoded."
  [url k v]
  (if (no-credentials? url)
    url
    (parser/put url k (percent/encode (chars/scalar-values v) percent/userinfo-set))))

(defn- with-part
  "The `url` with its query or fragment `k` set by the text `v`, without
  the `delimiter` that starts it, as the parser reads it in the `state`."
  [url k state delimiter v]
  (if (= "" v)
    (dissoc url k)
    (with-parse (assoc url k "") state (cond-> v (str/starts-with? v delimiter) (subs 1)))))

(defn- set-attribute
  "The `url` with the attribute `k` set to the text `v`, as its setter in
  the URL class sets it."
  [url k v]
  (let [opaque? (string? (:path url))]
    (case k
      :href     (or (parse v) url)
      :protocol (with-parse url :scheme-start (str v ":"))
      :username (with-userinfo url :username v)
      :password (with-userinfo url :password v)
      :host     (if opaque? url (with-parse url :host v))
      :hostname (if opaque? url (with-parse url :hostname v))
      :port     (cond
                  (no-credentials? url) url
                  (= "" v)              (dissoc url :port)
                  :else                 (with-parse url :port v))
      :pathname (if opaque? url (with-parse (assoc url :path []) :path-start v))
      :search   (with-part url :query :query "?" v)
      :hash     (with-part url :fragment :fragment "#" v)
      (throw (ex-info (str "The URL class has no attribute " k) {:attribute k})))))

(defn with
  "The `url` with each attribute `k` set to its value `v`, in order, as
  the setters of the URL class set them. The attributes are :href,
  :protocol, :username, :password, :host, :hostname, :port, :pathname,
  :search and :hash, which take text, and :search-params, which takes
  name-value pairs. A value that a setter refuses leaves the URL as it is.

      (with \"https://example.com/a?b\" :pathname \"/c d\" :search \"\")
      ;; => {:scheme \"https\" :host \"example.com\" :path [\"c%20d\"]}"
  [url k v & kvs]
  (when-let [url (->url url)]
    (in-order
     (reduce (fn [url [k v]]
               (if (= :search-params k)
                 (parser/put url :query (not-empty (params/serialize v)))
                 (set-attribute url k (str v))))
             url
             (partition 2 (list* k v kvs))))))

(defn- attribute
  "The value of the attribute `k` of the `url`, as its getter in the URL
  class gives it, or its name-value pairs for :search-params."
  [url k]
  (case k
    :href          (href url)
    :protocol      (protocol url)
    :username      (username url)
    :password      (password url)
    :host          (host url)
    :hostname      (hostname url)
    :port          (port url)
    :pathname      (pathname url)
    :search        (search url)
    :hash          (hash url)
    :search-params (search-params url)
    (throw (ex-info (str "The URL class has no attribute " k) {:attribute k}))))

(defn update
  "The `url` with the attribute `k` set to what `f` gives for its value and
  `args`, as with sets it. The attributes are those of with.

      (update \"https://example.com/?q=a\" :search-params params/set \"page\" 2)
      ;; => {:scheme \"https\" :host \"example.com\" :path [\"\"]
      ;;     :query \"q=a&page=2\"}"
  [url k f & args]
  (when-let [url (->url url)]
    (with url k (apply f (attribute url k) args))))

(comment
  (parse "HTTPS://EXAMPLE.com:443/a/./b/../c d?x y#z")
  (href (parse "../x" "https://example.com/a/b"))
  (resolve "https://example.com/blog/" "../about")
  (validation-errors "https:example.org")
  (origin "blob:https://whatwg.org/d0360e2f-caee-469f-9a2f-87d5b0456f6f")
  (with "https://example.com/" :host "example.org:8080" :hash "top")
  #_.)
