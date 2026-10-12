(ns ^:no-doc dk.simongray.whatwg-url.serializer
  "The URL serializer of the URL Standard, section 4.5, which writes a URL
  map as text. Its normal? tells whether the text of a URL is already
  written that way."
  (:require [clojure.string :as str]
            [dk.simongray.whatwg-url.chars :as chars]
            [dk.simongray.whatwg-url.hosts :as hosts]))

;; URL Standard, section 4.5, the URL path serializer
(defn serialize-path
  "The text of the `path` of a URL, e.g. /a/b, or the path itself when
  it's opaque."
  [path]
  (if (string? path)
    path
    ;; a builder rather than apply str of a lazy seq
    (str (reduce #(-> %1 (chars/append! "/") (chars/append! %2)) (chars/builder) path))))

;; URL Standard, section 4.5, the URL serializer
(defn serialize
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
         (serialize-path path)
         (when query (str "?" query))
         (when (and fragment? fragment) (str "#" fragment)))))

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

(defn- host-slash
  "The index of the / that ends a host of host-code? codes in the text `s`
  from `start`, or nil when another code ends it."
  [s ^long start]
  (let [n (count s)]
    (loop [i start]
      (when (< i n)
        (let [c (chars/code-at s i)]
          (cond
            (host-code? c) (recur (inc i))
            (== 0x2F c)    i))))))

(defn- normal-path?
  "Whether the path, query and fragment of the text `s` from `start`, just
  after a /, are all codes that path-code? takes."
  [s ^long start]
  (let [n (count s)]
    (loop [i start before 0x2F]
      (or (== i n)
          (let [c (chars/code-at s i)]
            (and (path-code? before c) (recur (inc i) c)))))))

;; most links of a page are written as they're serialized, which one pass
;; over them tells
(defn normal?
  "Whether the text `s` is an http or https URL that the parser and the
  serializer would return unchanged: with a host of lower-case letters,
  digits, dots and hyphens that doesn't end in a number, a path, and no
  port or credentials."
  [s]
  (let [start (cond
                (str/starts-with? s "https://") 8
                (str/starts-with? s "http://")  7)
        slash (some->> start (host-slash s))]
    (boolean
     (and slash
          (< start slash)
          (not (hosts/ends-in-a-number? (subs s start slash)))
          (normal-path? s (inc slash))))))
