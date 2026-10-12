(ns dk.simongray.whatwg-url
  "Parse, resolve, write and compare URLs the way browsers do, by the URL
  Standard.

  A URL is a plain map of the parts it has:

  - :scheme, in lower case, e.g. \"https\"
  - :username and :password, percent-encoded, when they aren't empty
  - :host, as text, e.g. \"example.com\", \"127.0.0.1\" or \"[::1]\" (it
    can be empty, as in file:///etc/hosts)
  - :port, a number, unless it's the default of the scheme
  - :path, a vector of segments, or a string for an opaque path like that
    of mailto:me@example.com
  - :query and :fragment, without their ? and # (both can be empty)

  Every function that takes a URL also takes its text (e.g. a string or a
  java.net.URI) and parses it as an absolute URL first. For text that
  doesn't parse, it returns nil.

  The functions named after the attributes of JavaScript's URL class (e.g.
  hostname and pathname) return their values. To set them the way that
  class's setters do, use with and update."
  (:refer-clojure :exclude [hash resolve update])
  (:require [clojure.string :as str]
            [dk.simongray.whatwg-url.params :as params]
            [dk.simongray.whatwg-url.parser :as parser]
            [dk.simongray.whatwg-url.serializer :as serializer]
            [dk.simongray.whatwg-url.setters :as setters]))

;; URL Standard, section 6.1, the API URL parser
(defn parse
  "The URL in the text `s`, resolved against the URL `base` when given, or
  nil when `s` doesn't parse. The base is a map or text.

      (parse \"../c?q#f\" \"https://EXAMPLE.com/a/b\")
      ;; => {:scheme \"https\" :host \"example.com\" :path [\"c\"]
      ;;     :query \"q\" :fragment \"f\"}"
  ([s]
   (parse s nil))
  ([s base]
   (let [base (parser/base-url base)]
     (when-not (= ::parser/invalid base)
       (some-> (parser/parse s base) parser/in-order)))))

(defn- ->url
  "The `url`, parsed when it's text."
  [url]
  (cond
    (nil? url) nil
    (map? url) url
    :else      (parse url)))

(defn href
  "The text of the `url`, with `opts`:

  - :fragment?, false to leave out the fragment

      (href {:scheme \"https\" :host \"example.com\" :path [\"\"]})
      ;; => \"https://example.com/\""
  ([url]
   (href url {}))
  ([url {:keys [fragment?] :or {fragment? true}}]
   (if (and (string? url) (serializer/normal? url))
     (cond-> url
       (and (not fragment?) (str/includes? url "#")) (subs 0 (str/index-of url "#")))
     (serializer/serialize (->url url) fragment?))))

(defn resolve
  "The text of the URL `s` resolved against the URL `base`, or nil when it
  doesn't parse. This is how a browser resolves a link against the URL of
  its page. With a nil base, only an absolute URL parses.

      (resolve \"https://example.com/blog/post\" \"../about\")
      ;; => \"https://example.com/about\"

  For many URLs against the same base, use resolver, which parses the base
  only once."
  [base s]
  (let [base (parser/base-url base)]
    (cond
      (= ::parser/invalid base)    nil
      (nil? s)                     nil
      (serializer/normal? (str s)) (str s)
      :else                        (some-> (parse s base) (serializer/serialize true)))))

(defn resolver
  "A function that resolves a URL against the URL `base` as resolve does,
  but parses the base only once, e.g. for the links of a page.

      (map (resolver \"https://example.com/blog/post\") [\"a\" \"/b\"])
      ;; => (\"https://example.com/blog/a\" \"https://example.com/b\")"
  [base]
  (let [base (parser/base-url base)]
    (fn [s]
      (when-not (= ::parser/invalid base)
        (resolve base s)))))

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

(defn host
  "The host and port of the `url`, e.g. example.com:8443, or an empty
  string when it has no host."
  [url]
  (when-let [{:keys [host port]} (->url url)]
    (str host (when (and host port) (str ":" port)))))

(defn hostname
  "The host of the `url`, e.g. example.com, or an empty string when it has
  none."
  [url]
  (some-> (->url url) (:host "")))

(defn port
  "The port of the `url` as text, e.g. 8443, or an empty string when it's
  the scheme's default or there's none."
  [url]
  (some-> (->url url) (:port "") str))

(defn pathname
  "The path of the `url` as text, e.g. /a/b."
  [url]
  (when-let [url (->url url)]
    (serializer/serialize-path (:path url))))

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
  "The name-value pairs of the query of the `url`, as the params namespace
  takes them."
  [url]
  (some-> (->url url) (:query "") params/parse))

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

(defn with
  "The `url` with each attribute `k` set to its value `v` in order, as the
  setters of the URL class set them. A setter that refuses a value leaves
  the URL as it is.

  The attributes :href, :protocol, :username, :password, :host, :hostname,
  :port, :pathname, :search and :hash take text. The attribute
  :search-params takes name-value pairs.

      (with \"https://example.com/a?b\" :pathname \"/c d\" :search \"\")
      ;; => {:scheme \"https\" :host \"example.com\" :path [\"c%20d\"]}"
  [url k v & kvs]
  (some-> (->url url) (setters/set-attributes k v kvs)))

(defn- attribute
  "The value of the attribute `k` of the `url`, as its getter in the URL
  class returns it, or its name-value pairs for :search-params."
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
    (throw (setters/no-attribute k))))

(defn update
  "The `url` with the attribute `k` set to what `f` returns for its value
  and the `args`, as with sets it. The attributes are those of with.

      (update \"https://example.com/?q=a\" :search-params params/set \"page\" 2)
      ;; => {:scheme \"https\" :host \"example.com\" :path [\"\"]
      ;;     :query \"q=a&page=2\"}"
  [url k f & args]
  (when-let [url (->url url)]
    (with url k (apply f (attribute url k) args))))

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
  `prefix`, the way cookies compare paths. So /a/b starts with /a, but /ab
  doesn't. An opaque path starts with nothing."
  [url prefix]
  (let [a (:path (->url url))
        b (:path (->url prefix))]
    (boolean (and (vector? a) (vector? b)
                  (<= (count b) (count a))
                  (= b (subvec a 0 (count b)))))))

(defn special?
  "Whether the `url` has a special scheme: ftp, file, http, https, ws or
  wss."
  [url]
  (boolean (some-> (->url url) :scheme parser/special?)))

(defn validation-errors
  "The validation errors of the text `s` as a URL resolved against `base`,
  or nil when the base doesn't parse.

  The errors are keywords in the order they're found, named as in section
  1.1 of the URL Standard, e.g. :invalid-credentials. When `s` doesn't
  parse, the last error is the one that made it fail (if there is one).

      (validation-errors \"https://user@example.org/a b\")
      ;; => [:invalid-credentials :invalid-url-unit]"
  ([s]
   (validation-errors s nil))
  ([s base]
   (some-> (parser/report s base) :errors distinct vec)))

(defn valid?
  "Whether the text `s` is a valid URL string, resolved against `base`:
  one that parses with no validation error."
  ([s]
   (valid? s nil))
  ([s base]
   (let [{:keys [state errors]} (parser/report s base)]
     (and (= :done state) (empty? errors)))))

(comment
  (parse "HTTPS://EXAMPLE.com:443/a/./b/../c d?x y#z")
  (href (parse "../x" "https://example.com/a/b"))
  (resolve "https://example.com/blog/" "../about")
  (validation-errors "https:example.org")
  (origin "blob:https://whatwg.org/d0360e2f-caee-469f-9a2f-87d5b0456f6f")
  (with "https://example.com/" :host "example.org:8080" :hash "top")
  #_.)
