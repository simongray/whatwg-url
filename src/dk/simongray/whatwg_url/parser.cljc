(ns dk.simongray.whatwg-url.parser
  "The basic URL parser of the URL Standard, section 4.4, as a state
  machine whose states each read a run of the input rather than a code
  point at a time.

  A URL is a map of :scheme, :username, :password, :host, :port, :path,
  :query and :fragment, with only the keys that have a value: no empty
  username or password, and no null host, port, query or fragment. The
  :path is a vector of segments, or a string for an opaque path, e.g. that
  of mailto:me@example.com.

  Each state is a function of the parse, a map of the :input, the pointer
  :i, the :url so far, the :base, the :override state of a setter and the
  :errors, which gives the parse of the next state, the :done or the
  :failure. The :errors are a vector of the validation errors when they're
  asked for, and nil otherwise.

  TODO: the encodings other than UTF-8 that HTML gives the parser for the
  query of a document in a legacy encoding."
  (:require [clojure.string :as str]
            [dk.simongray.whatwg-url.chars :as chars]
            [dk.simongray.whatwg-url.hosts :as hosts]
            [dk.simongray.whatwg-url.percent :as percent]))

;; URL Standard, section 4.2
(def default-ports
  "The special schemes, each with its default port, or nil."
  {"ftp" 21 "file" nil "http" 80 "https" 443 "ws" 80 "wss" 443})

(defn special?
  "Whether the `scheme` is special."
  [scheme]
  (contains? default-ports scheme))

(defn put
  "The `url` with its key `k` set to `v`, or without it when `v` is nil,
  or an empty username or password."
  [url k v]
  (if (or (nil? v) (and (= "" v) (#{:username :password} k)))
    (dissoc url k)
    (assoc url k v)))

(defn credentials?
  "Whether the `url` has a username or a password."
  [url]
  (boolean (or (:username url) (:password url))))

;; URL Standard, section 4.2, "shorten a url's path"
(defn shorten
  "The `path` of a URL with the `scheme` without its last segment, unless
  it's the drive letter alone of a file URL."
  [scheme path]
  (if (and (= "file" scheme)
           (= 1 (count path))
           (re-matches #"[A-Za-z]:" (first path)))
    path
    (cond-> path (seq path) pop)))

(defn- error
  "The `parse` with the validation error `k` added, when it collects them."
  [parse k]
  (cond-> parse (:errors parse) (update :errors conj k)))

(defn- c
  "The code point at `i` of the `input`, or -1 at its end."
  ^long [input ^long i]
  (if (< i (count input)) (chars/code-point-at input i) -1))

(defn- url-special?
  "Whether the URL of the `parse` has a special scheme."
  [parse]
  (special? (:scheme (:url parse))))

(defn- slash?
  "Whether `x` is a slash of a URL that's `special`: / or, when it's
  special, \\."
  [special ^long x]
  (or (== x 0x2F) (and (== x 0x5C) special)))

(defn- end-of-authority?
  "Whether `x` ends the authority or host of a URL that's `special`."
  [special ^long x]
  (or (== x -1) (== x 0x3F) (== x 0x23) (slash? special x)))

(defn- scan
  "The index of the first code point of the `input` from `i` that `pred`
  holds for, or the end of the input."
  [input ^long i pred]
  (let [n (count input)]
    (loop [i i]
      (if (< i n)
        (let [x (chars/code-point-at input i)]
          (if (pred x)
            i
            (recur (+ i (chars/width x)))))
        n))))

(defn- go
  "The `parse` in the `state` at `i`."
  [parse state i]
  (assoc parse :state state :i i))

(defn- done
  "The `parse` that has ended with its URL."
  [parse]
  (assoc parse :state :done))

(defn- failure
  "The `parse` that has failed, with the validation error `k` when given."
  ([parse]
   (assoc parse :state :failure))
  ([parse k]
   (failure (error parse k))))

(defn- url-units
  "The `parse` with an invalid-url-unit error when the input from `start`
  to `end` isn't all URL units."
  [parse start end]
  (cond-> parse
    (and (:errors parse) (not (percent/url-units? (:input parse) start end)))
    (error :invalid-url-unit)))

(defn- starts-with-drive-letter?
  "Whether the `input` starts with a Windows drive letter at `i`."
  [input i]
  (let [x (c input i) y (c input (inc i)) z (c input (+ i 2))]
    (and (chars/ascii-alpha? x)
         (or (== y 0x3A) (== y 0x7C))
         (or (== z -1) (== z 0x2F) (== z 0x5C) (== z 0x3F) (== z 0x23)))))

(defn- drive-letter?
  "Whether the text `s` is a Windows drive letter, normalized when
  `normalized?`."
  [s normalized?]
  (boolean (re-matches (if normalized? #"[A-Za-z]:" #"[A-Za-z][:|]") s)))

;; scheme start state
(defn- scheme-start
  "The `parse` from the scheme start state."
  [{:keys [input i override] :as parse}]
  (cond
    (chars/ascii-alpha? (c input i)) (go parse :scheme i)
    (not override)                   (go parse :no-scheme i)
    :else                            (failure parse)))

(defn- scheme-override
  "The `parse` of a setter that sets the `scheme`, which it may refuse."
  [{:keys [url] :as parse} scheme]
  (if (or (not= (special? (:scheme url)) (special? scheme))
          (and (or (credentials? url) (:port url)) (= "file" scheme))
          (and (= "file" (:scheme url)) (= "" (:host url))))
    (done parse)
    (done (update parse :url #(cond-> (assoc % :scheme scheme)
                                (= (:port %) (default-ports scheme)) (dissoc :port))))))

(defn- scheme-code-point?
  "Whether `x` can be in a scheme after its first letter."
  [^long x]
  (or (chars/ascii-alphanumeric? x) (== x 0x2B) (== x 0x2D) (== x 0x2E)))

;; scheme state
(defn- scheme
  "The `parse` from the scheme state."
  [{:keys [input i override base] :as parse}]
  (let [j (scan input i #(not (scheme-code-point? %)))]
    (cond
      (and (== 0x3A (c input j)) override)
      (scheme-override parse (chars/lower-case (subs input i j)))

      (== 0x3A (c input j))
      (let [s     (chars/lower-case (subs input i j))
            parse (assoc-in parse [:url :scheme] s)
            next  (inc j)]
        (cond
          (= "file" s)
          (-> (cond-> parse
                (not (and (== 0x2F (c input next)) (== 0x2F (c input (inc next)))))
                (error :special-scheme-missing-following-solidus))
              (go :file next))

          (and (special? s) base (= s (:scheme base)))
          (go parse :special-relative-or-authority next)

          (special? s)
          (go parse :special-authority-slashes next)

          (== 0x2F (c input next))
          (go parse :path-or-authority (inc next))

          :else
          (go (assoc-in parse [:url :path] "") :opaque-path next)))

      (not override)
      (go parse :no-scheme 0)

      :else
      (failure parse))))

;; no scheme state
(defn- no-scheme
  "The `parse` from the no scheme state."
  [{:keys [input i base] :as parse}]
  (let [opaque? (string? (:path base))]
    (cond
      (or (nil? base) (and opaque? (not= 0x23 (c input i))))
      (failure parse :missing-scheme-non-relative-url)

      opaque?
      (go (assoc parse :url (-> (select-keys base [:scheme :path :query])
                                (assoc :fragment "")))
          :fragment (inc i))

      (not= "file" (:scheme base))
      (go parse :relative i)

      :else
      (go parse :file i))))

;; special relative or authority state
(defn- special-relative-or-authority
  "The `parse` from the special relative or authority state."
  [{:keys [input i] :as parse}]
  (if (and (== 0x2F (c input i)) (== 0x2F (c input (inc i))))
    (go parse :special-authority-ignore-slashes (+ i 2))
    (-> parse
        (error :special-scheme-missing-following-solidus)
        (go :relative i))))

;; path or authority state
(defn- path-or-authority
  "The `parse` from the path or authority state."
  [{:keys [input i] :as parse}]
  (if (== 0x2F (c input i))
    (go parse :authority (inc i))
    (go parse :path i)))

(defn- from-base
  "The URL of the `parse` with the keys `ks` of its base."
  [{:keys [base] :as parse} ks]
  (update parse :url #(reduce (fn [url k] (put url k (get base k))) % ks)))

;; relative state
(defn- relative
  "The `parse` from the relative state."
  [{:keys [input i base] :as parse}]
  (let [parse (assoc-in parse [:url :scheme] (:scheme base))
        x     (c input i)]
    (cond
      (== 0x2F x)
      (go parse :relative-slash (inc i))

      (and (url-special? parse) (== 0x5C x))
      (-> parse (error :invalid-reverse-solidus) (go :relative-slash (inc i)))

      :else
      (let [parse (from-base parse [:username :password :host :port :path :query])]
        (case x
          0x3F (go (assoc-in parse [:url :query] "") :query (inc i))
          0x23 (go (assoc-in parse [:url :fragment] "") :fragment (inc i))
          -1   (done parse)
          (-> parse
              (update :url #(-> (dissoc % :query)
                                (update :path (partial shorten (:scheme %)))))
              (go :path i)))))))

;; relative slash state
(defn- relative-slash
  "The `parse` from the relative slash state."
  [{:keys [input i] :as parse}]
  (let [x (c input i)]
    (cond
      (and (url-special? parse) (or (== 0x2F x) (== 0x5C x)))
      (-> (cond-> parse (== 0x5C x) (error :invalid-reverse-solidus))
          (go :special-authority-ignore-slashes (inc i)))

      (== 0x2F x)
      (go parse :authority (inc i))

      :else
      (-> parse
          (from-base [:username :password :host :port])
          (go :path i)))))

;; special authority slashes state
(defn- special-authority-slashes
  "The `parse` from the special authority slashes state."
  [{:keys [input i] :as parse}]
  (if (and (== 0x2F (c input i)) (== 0x2F (c input (inc i))))
    (go parse :special-authority-ignore-slashes (+ i 2))
    (-> parse
        (error :special-scheme-missing-following-solidus)
        (go :special-authority-ignore-slashes i))))

;; special authority ignore slashes state
(defn- special-authority-ignore-slashes
  "The `parse` from the special authority ignore slashes state."
  [{:keys [input i] :as parse}]
  (let [j (scan input i #(not (or (== 0x2F %) (== 0x5C %))))]
    (-> (cond-> parse (< i j) (error :special-scheme-missing-following-solidus))
        (go :authority j))))

(defn- userinfo
  "The URL of the `parse` with the username and password of the text `s`,
  percent-encoded."
  [parse s]
  (let [[username password] (str/split s #":" 2)
        encode              #(percent/encode % percent/userinfo-set)]
    (update parse :url #(-> %
                            (put :username (encode username))
                            (put :password (some-> password encode))))))

;; authority state
(defn- authority
  "The `parse` from the authority state."
  [{:keys [input i] :as parse}]
  (let [special (url-special? parse)
        end     (scan input i #(end-of-authority? special %))
        at      (some-> (str/last-index-of (subs input i end) "@") (+ i))]
    (cond
      (nil? at)
      (go parse :host i)

      (== (inc at) end)
      (failure (error parse :invalid-credentials) :host-missing)

      :else
      (-> parse
          (error :invalid-credentials)
          (userinfo (subs input i at))
          (go :host (inc at))))))

(defn- with-host
  "The `parse` with the host of the text `s` set, or its failure."
  [parse s]
  (let [[host errors] (hosts/parse-report s (not (url-special? parse)) (:errors parse))
        parse         (assoc parse :errors errors)]
    (if host
      (assoc-in parse [:url :host] host)
      (failure parse))))

(defn- host-end
  "The index in the `input` from `i` where the host of a URL that's
  `special` ends: the end of the authority, or a colon outside brackets."
  [input i special]
  (let [n (count input)]
    (loop [j i brackets? false]
      (if (< j n)
        (let [x (chars/code-point-at input j)]
          (cond
            (end-of-authority? special x)     j
            (and (== 0x3A x) (not brackets?)) j
            :else                             (recur (+ j (chars/width x))
                                                     (case (int x)
                                                       0x5B true
                                                       0x5D false
                                                       brackets?))))
        n))))

;; host state and hostname state
(defn- host
  "The `parse` from the host state, or the hostname state of a setter."
  [{:keys [input i override url] :as parse}]
  (if (and override (= "file" (:scheme url)))
    (go parse :file-host i)
    (let [special (url-special? parse)
          j       (host-end input i special)
          buffer  (subs input i j)]
      (cond
        (== 0x3A (c input j))
        (cond
          (= "" buffer)          (failure parse :host-missing)
          (= :hostname override) (failure parse)
          :else                  (let [parse (with-host parse buffer)]
                                   (cond-> parse
                                     (not= :failure (:state parse)) (go :port (inc j)))))

        (and special (= "" buffer))
        (failure parse :host-missing)

        (and override (= "" buffer) (or (credentials? url) (:port url)))
        (failure parse)

        :else
        (let [parse (with-host parse buffer)]
          (cond
            (= :failure (:state parse)) parse
            override                    (done parse)
            :else                       (go parse :path-start j)))))))

(defn- port-number
  "The number of the ASCII `digits`, or 65536 when it's greater than that."
  [digits]
  (loop [k 0 n 0]
    (if (and (< k (count digits)) (<= n 65535))
      (recur (inc k) (+ (* n 10) (- (chars/code-at digits k) 0x30)))
      (min n 65536))))

;; port state
(defn- port
  "The `parse` from the port state."
  [{:keys [input i override] :as parse}]
  (let [j      (scan input i #(not (chars/ascii-digit? %)))
        digits (subs input i j)]
    (cond
      (not (or (end-of-authority? (url-special? parse) (c input j)) override))
      (failure parse :port-invalid)

      (= "" digits)
      (if override (failure parse) (go parse :path-start j))

      :else
      (let [n (port-number digits)]
        (if (< 65535 n)
          (failure parse :port-out-of-range)
          (let [parse (update parse :url
                              #(put % :port (when-not (= n (default-ports (:scheme %))) n)))]
            (if override
              (done parse)
              (go parse :path-start j))))))))

;; file state
(defn- file
  "The `parse` from the file state."
  [{:keys [input i base] :as parse}]
  (let [parse (update parse :url assoc :scheme "file" :host "")
        x     (c input i)]
    (cond
      (or (== 0x2F x) (== 0x5C x))
      (-> (cond-> parse (== 0x5C x) (error :invalid-reverse-solidus))
          (go :file-slash (inc i)))

      (= "file" (:scheme base))
      (let [parse (from-base parse [:host :path :query])]
        (case x
          0x3F (go (assoc-in parse [:url :query] "") :query (inc i))
          0x23 (go (assoc-in parse [:url :fragment] "") :fragment (inc i))
          -1   (done parse)
          (-> (if (starts-with-drive-letter? input i)
                (-> parse
                    (error :file-invalid-windows-drive-letter)
                    (update :url assoc :path []))
                (update-in parse [:url :path] #(shorten "file" %)))
              (update :url dissoc :query)
              (go :path i))))

      :else
      (go parse :path i))))

;; file slash state
(defn- file-slash
  "The `parse` from the file slash state."
  [{:keys [input i base] :as parse}]
  (let [x (c input i)]
    (cond
      (or (== 0x2F x) (== 0x5C x))
      (-> (cond-> parse (== 0x5C x) (error :invalid-reverse-solidus))
          (go :file-host (inc i)))

      (= "file" (:scheme base))
      (let [drive (first (:path base))
            parse (from-base parse [:host])]
        (-> (cond-> parse
              (and (not (starts-with-drive-letter? input i))
                   (string? drive)
                   (drive-letter? drive true))
              (update-in [:url :path] (fnil conj []) drive))
            (go :path i)))

      :else
      (go parse :path i))))

;; file host state
(defn- file-host
  "The `parse` from the file host state."
  [{:keys [input i override] :as parse}]
  (let [j      (scan input i #(or (== 0x2F %) (== 0x5C %) (== 0x3F %) (== 0x23 %)))
        buffer (subs input i j)]
    (cond
      (and (not override) (drive-letter? buffer false))
      (-> parse
          (error :file-invalid-windows-drive-letter-host)
          (assoc :buffer buffer)
          (go :path j))

      (= "" buffer)
      (let [parse (assoc-in parse [:url :host] "")]
        (if override (done parse) (go parse :path-start j)))

      :else
      (let [parse (with-host parse buffer)]
        (if (= :failure (:state parse))
          parse
          (let [parse (cond-> parse
                        (= "localhost" (:host (:url parse))) (assoc-in [:url :host] ""))]
            (if override (done parse) (go parse :path-start j))))))))

;; path start state
(defn- path-start
  "The `parse` from the path start state."
  [{:keys [input i override] :as parse}]
  (let [x (c input i)]
    (cond
      (url-special? parse)
      (-> (cond-> parse (== 0x5C x) (error :invalid-reverse-solidus))
          (go :path (if (or (== 0x2F x) (== 0x5C x)) (inc i) i)))

      (and (not override) (== 0x3F x))
      (go (assoc-in parse [:url :query] "") :query (inc i))

      (and (not override) (== 0x23 x))
      (go (assoc-in parse [:url :fragment] "") :fragment (inc i))

      (not= -1 x)
      (go parse :path (if (== 0x2F x) (inc i) i))

      (and override (nil? (:host (:url parse))))
      (done (update-in parse [:url :path] (fnil conj []) ""))

      :else
      (done parse))))

(defn- dot-segment
  "The kind of dot segment that the path `segment` is, :single, e.g. . or
  %2e, or :double, e.g. .. or %2e%2e, or nil for another segment."
  [segment]
  (when (and (<= 1 (count segment) 6)
             (let [x (chars/code-at segment 0)] (or (== x 0x2E) (== x 0x25))))
    (case (chars/lower-case segment)
      ("." "%2e")                   :single
      (".." ".%2e" "%2e." "%2e%2e") :double
      nil)))

(defn- with-segment
  "The `path` of a URL with the `scheme` with the `segment` added, which a
  delimiter `slash?` ends rather than the end of the path."
  [scheme path segment slash?]
  (case (dot-segment segment)
    :double (cond-> (shorten scheme path) (not slash?) (conj ""))
    :single (cond-> path (not slash?) (conj ""))
    (if (and (= "file" scheme) (empty? path) (drive-letter? segment false))
      (conj path (str (subs segment 0 1) ":"))
      (conj path segment))))

;; path state
(defn- path
  "The `parse` from the path state, which reads all the segments of the
  path."
  [{:keys [input i override buffer url] :as parse}]
  (let [special    (url-special? parse)
        delimiter? #(or (slash? special %)
                        (and (not override) (or (== 0x3F %) (== 0x23 %))))]
    (loop [i i buffer buffer path (vec (:path url)) parse parse]
      (let [j       (scan input i delimiter?)
            x       (c input j)
            segment (cond->> (percent/encode-span percent/path-set input i j)
                      buffer (str buffer))
            parse   (cond-> (url-units parse i j)
                      (and special (== 0x5C x)) (error :invalid-reverse-solidus))
            path    (with-segment (:scheme url) path segment (slash? special x))]
        (if (slash? special x)
          (recur (inc j) nil path parse)
          (let [parse (-> parse (dissoc :buffer) (assoc-in [:url :path] path))]
            (case x
              -1   (done parse)
              0x3F (go (assoc-in parse [:url :query] "") :query (inc j))
              (go (assoc-in parse [:url :fragment] "") :fragment (inc j)))))))))

;; opaque path state
(defn- opaque-path
  "The `parse` from the opaque path state."
  [{:keys [input i] :as parse}]
  (let [j     (scan input i #(or (== 0x3F %) (== 0x23 %)))
        span  (percent/encode-span percent/c0-control-set input i j)
        ;; a space before the query or the fragment is encoded, so that
        ;; the path doesn't end in one
        span  (cond-> span
                (and (< i j) (not= -1 (c input j)) (str/ends-with? span " "))
                (-> (subs 0 (dec (count span))) (str "%20")))
        parse (-> (url-units parse i j)
                  (update-in [:url :path] str span))]
    (case (c input j)
      -1   (done parse)
      0x3F (go (assoc-in parse [:url :query] "") :query (inc j))
      (go (assoc-in parse [:url :fragment] "") :fragment (inc j)))))

;; query state
(defn- query
  "The `parse` from the query state."
  [{:keys [input i override] :as parse}]
  (let [j     (if override (count input) (scan input i #(== 0x23 %)))
        set   (if (url-special? parse) percent/special-query-set percent/query-set)
        parse (-> (url-units parse i j)
                  (update-in [:url :query] str (percent/encode-span set input i j)))]
    (if (== 0x23 (c input j))
      (go (assoc-in parse [:url :fragment] "") :fragment (inc j))
      (done parse))))

;; fragment state
(defn- fragment
  "The `parse` from the fragment state."
  [{:keys [input i] :as parse}]
  (let [j (count input)]
    (-> (url-units parse i j)
        (update-in [:url :fragment] str (percent/encode-span percent/fragment-set input i j))
        done)))

(def states
  "The function of each state of the parser."
  {:scheme-start                     scheme-start
   :scheme                           scheme
   :no-scheme                        no-scheme
   :special-relative-or-authority    special-relative-or-authority
   :path-or-authority                path-or-authority
   :relative                         relative
   :relative-slash                   relative-slash
   :special-authority-slashes        special-authority-slashes
   :special-authority-ignore-slashes special-authority-ignore-slashes
   :authority                        authority
   :host                             host
   :hostname                         host
   :port                             port
   :file                             file
   :file-slash                       file-slash
   :file-host                        file-host
   :path-start                       path-start
   :path                             path
   :opaque-path                      opaque-path
   :query                            query
   :fragment                         fragment})

(defn- clean?
  "Whether the `input` of a parse has no tab or line break, and when
  `trim?` no C0 control or space at its ends."
  [input trim?]
  (let [n (count input)]
    (and (or (not trim?)
             (zero? n)
             (and (< 0x20 (chars/code-at input 0))
                  (< 0x20 (chars/code-at input (dec n)))))
         (loop [i 0]
           (cond
             (== i n) true
             (let [x (chars/code-at input i)] (or (== x 0x09) (== x 0x0A) (== x 0x0D))) false
             :else (recur (inc i)))))))

(defn- cleaned
  "The `input` of a parse with its tabs and line breaks removed, and when
  `trim?` the C0 controls and spaces at its ends, and `errors` with the
  validation error of each, as a pair."
  [input trim? errors]
  (if (clean? input trim?)
    [input errors]
    (let [trimmed (if trim?
                    (str/replace input #"^[\x00-\x20]+|[\x00-\x20]+$" "")
                    input)
          clean   (str/replace trimmed #"[\t\n\r]" "")]
      [clean (cond-> errors
               (not= input trimmed) (some-> (conj :invalid-url-unit))
               (not= trimmed clean) (some-> (conj :invalid-url-unit)))])))

(defn run
  "The parse of the text `input` by the basic URL parser, with `opts`:

  - :base, the URL that a relative URL is resolved against
  - :url and :override, the URL that a setter changes and the state it
    starts in, e.g. :hostname
  - :errors?, true to collect the validation errors

  The parse is a map with the :url, the :state, which is :done or
  :failure, and the :errors when they're collected. A setter can change
  the URL before it fails."
  [input {:keys [base url override errors?]}]
  (let [[input errors] (cleaned (chars/scalar-values input)
                                (nil? url)
                                (when errors? []))]
    (loop [parse {:input    input
                  :i        0
                  :url      (or url {:path []})
                  :base     base
                  :override override
                  :errors   errors
                  :state    (or override :scheme-start)}]
      (case (:state parse)
        (:done :failure) parse
        (recur ((states (:state parse)) parse))))))
