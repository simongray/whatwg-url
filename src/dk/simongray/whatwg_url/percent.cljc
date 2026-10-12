(ns dk.simongray.whatwg-url.percent
  "Percent-encoding and percent-decoding, by the percent-encode sets of
  the URL Standard, section 1.3.

  Each set is a predicate of a code point, so a set of your own is a
  function too, e.g. (some-fn percent/path-set #{0x3B}). Every set of the
  standard holds the code points above U+007E.

  TODO: the encodings other than UTF-8 that HTML gives the parser for the
  query of a document in a legacy encoding."
  (:require [clojure.string :as str]
            [dk.simongray.whatwg-url.chars :as chars]))

(defn- encode-set
  "The percent-encode set of the C0 controls, the code points above U+007E
  and the ASCII characters of the text `s`."
  [s]
  (let [^booleans table #?(:clj  (boolean-array 128)
                           :cljs (.fill (make-array 128) false))]
    (doseq [c (concat (range 0x20) (map #(chars/code-at s %) (range (count s))))]
      (aset table c true))
    (fn [^long c]
      (or (< 0x7E c) (aget table c)))))

;; URL Standard, section 1.3, where each set holds the one before it
(def c0-control-set
  "The C0 control percent-encode set."
  (encode-set ""))

(def fragment-set
  "The fragment percent-encode set."
  (encode-set " \"<>`"))

(def query-set
  "The query percent-encode set."
  (encode-set " \"#<>"))

(def special-query-set
  "The special-query percent-encode set, for the query of a URL with a
  special scheme, e.g. https."
  (encode-set " \"#<>'"))

(def path-set
  "The path percent-encode set."
  (encode-set " \"#<>?^`{}"))

(def userinfo-set
  "The userinfo percent-encode set."
  (encode-set " \"#<>?^`{}/:;=@[\\]|"))

(def component-set
  "The component percent-encode set, which gives what JavaScript's
  encodeURIComponent gives."
  (encode-set " \"#<>?^`{}/:;=@[\\]|$%&+,"))

(def form-urlencoded-set
  "The application/x-www-form-urlencoded percent-encode set, which leaves
  only the ASCII letters and digits and * - . _ as they are. Encoding by
  it makes a space a +."
  (encode-set " \"#<>?^`{}/:;=@[\\]|$%&+,!'()~"))

(def ^:no-doc escapes
  "The percent-encoding of each byte, by its value, e.g. %C3 for 0xC3."
  (let [hex "0123456789ABCDEF"]
    (mapv #(str "%" (nth hex (quot % 16)) (nth hex (rem % 16))) (range 256))))

(defn ^:no-doc encode-span
  "The code points of the text `s` from `start` to `end` with each one of
  the percent-encode `set` percent-encoded as UTF-8, a space as + when the
  set is form-urlencoded-set."
  [set s ^long start ^long end]
  (let [plus? (identical? set form-urlencoded-set)]
    (if (loop [i start]
          (cond
            (== i end)                     true
            (set (chars/code-at s i))      false
            :else                          (recur (inc i))))
      (subs s start end)
      (let [b (chars/builder)]
        (loop [i start]
          (when (< i end)
            (let [c (chars/code-point-at s i)]
              (cond
                (and plus? (== c 0x20)) (chars/append! b "+")
                (set c)                 (run! #(chars/append! b (nth escapes %))
                                              (chars/utf-8 c))
                :else                   (chars/append-code-point! b c))
              (recur (+ i (chars/width c))))))
        (str b)))))

(defn encode
  "The text `s` with each code point of the percent-encode `set`
  percent-encoded as UTF-8, by component-set when no set is given.

      (encode \"Say what‽\")
      ;; => \"Say%20what%E2%80%BD\"

  By form-urlencoded-set a space becomes a +, as in the body of a form."
  ([s]
   (encode s component-set))
  ([s set]
   (encode-span set s 0 (count s))))

(defn ^:no-doc percent-encoded-byte-at?
  "Whether the text `s` has a percent-encoded byte at `i`, e.g. %2F."
  [s ^long i]
  (and (< (+ i 2) (count s))
       (== 0x25 (chars/code-at s i))
       (chars/ascii-hex-digit? (chars/code-at s (inc i)))
       (chars/ascii-hex-digit? (chars/code-at s (+ i 2)))))

;; URL Standard, section 4.3, "URL units"
(defn ^:no-doc url-units?
  "Whether the text `s` from `start` to `end` is all URL units: URL code
  points and percent-encoded bytes."
  [s ^long start ^long end]
  (loop [i start]
    (if (< i end)
      (let [c (chars/code-point-at s i)]
        (cond
          (== 0x25 c)               (and (percent-encoded-byte-at? s i) (recur (+ i 3)))
          (chars/url-code-point? c) (recur (+ i (chars/width c)))
          :else                     false))
      true)))

(defn decode-bytes
  "The bytes of the text `s` in UTF-8, with each percent-encoded byte
  decoded, as a vector of numbers. A % that starts no percent-encoded byte
  stays a %."
  [s]
  (let [n (count s)]
    (loop [i 0 out (transient [])]
      (if (< i n)
        (if (percent-encoded-byte-at? s i)
          (recur (+ i 3)
                 (conj! out (+ (* 16 (chars/hex-value (chars/code-at s (inc i))))
                               (chars/hex-value (chars/code-at s (+ i 2))))))
          (let [c (chars/code-point-at s i)]
            (recur (+ i (chars/width c)) (reduce conj! out (chars/utf-8 c)))))
        (persistent! out)))))

(defn decode
  "The text `s` with its percent-encoded bytes decoded as UTF-8, with
  U+FFFD for each sequence that isn't UTF-8. A % that starts no
  percent-encoded byte stays a %.

      (decode \"%E2%80%BD%25%2E\")
      ;; => \"‽%.\""
  [s]
  (if (str/includes? s "%")
    (chars/utf-8-decode (decode-bytes s))
    s))

(comment
  (encode "Say what‽" userinfo-set)
  (encode "1+1 ≡ 2%20‽" form-urlencoded-set)
  (decode "%25%s%1G")
  (decode "%FE%FF")
  #_.)
