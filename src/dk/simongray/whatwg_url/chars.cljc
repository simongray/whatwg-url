(ns ^:no-doc dk.simongray.whatwg-url.chars
  "Code points, the classes of ASCII that the URL Standard names, and the
  bytes of UTF-8, the same way on both platforms.

  The UTF-8 decoder is that of the Encoding Standard, which replaces each
  invalid sequence with U+FFFD as browsers do."
  #?(:clj (:import [java.util Locale])))

;; a primitive long, which the JVM doesn't box as it does a character
(defn code-at
  "The UTF-16 code unit at `i` of the text `s`."
  ^long [s ^long i]
  #?(:clj  (long (int (.charAt ^String s (int i))))
     :cljs (.charCodeAt s i)))

(defn code-point-at
  "The code point at `i` of the text `s`, or the code unit there when it's
  a surrogate without its pair."
  ^long [s ^long i]
  #?(:clj  (long (.codePointAt ^String s (int i)))
     :cljs (.codePointAt s i)))

(defn width
  "The number of UTF-16 code units of the code point `c`."
  ^long [^long c]
  (if (< c 0x10000) 1 2))

(defn surrogate?
  "Whether `c` is a surrogate code point."
  [^long c]
  (and (<= 0xD800 c) (<= c 0xDFFF)))

(defn code-points
  "The code points of the text `s`, as a vector."
  [s]
  (loop [i 0 out (transient [])]
    (if (< i (count s))
      (let [c (code-point-at s i)]
        (recur (+ i (width c)) (conj! out c)))
      (persistent! out))))

(defn builder
  "A builder of text, for append! and str."
  []
  #?(:clj  (StringBuilder.)
     :cljs (goog.string.StringBuffer.)))

(defn append!
  "The `builder` with the text `s` appended."
  [builder s]
  #?(:clj  (.append ^StringBuilder builder ^String s)
     :cljs (.append builder s)))

(defn append-code-point!
  "The `builder` with the code point `c` appended."
  [builder ^long c]
  #?(:clj  (.appendCodePoint ^StringBuilder builder (int c))
     :cljs (.append builder (js/String.fromCodePoint c))))

(defn from-code-points
  "The text of the code points `cps`."
  [cps]
  (let [b (builder)]
    (doseq [c cps]
      (append-code-point! b c))
    (str b)))

(defn scalar-values
  "The text `s` with each surrogate that has no pair replaced by U+FFFD, as
  Web IDL converts a USVString."
  [s]
  (let [n (count s)]
    (if (loop [i 0]
          (or (== i n)
              (let [c (code-point-at s i)]
                (and (not (surrogate? c)) (recur (+ i (width c)))))))
      s
      (let [b (builder)]
        (loop [i 0]
          (when (< i n)
            (let [c (code-point-at s i)]
              (append-code-point! b (if (surrogate? c) 0xFFFD c))
              (recur (+ i (width c))))))
        (str b)))))

(defn ascii?
  "Whether the text `s` is all ASCII."
  [s]
  (let [n (count s)]
    (loop [i 0]
      (cond
        (== i n)               true
        (< 0x7F (code-at s i)) false
        :else                  (recur (inc i))))))

;; str/lower-case follows the default locale on the JVM, which in Turkish
;; makes I a dotless i
(defn lower-case
  "The ASCII text `s` in lower case, whatever the locale."
  [s]
  #?(:clj  (.toLowerCase ^String s Locale/ROOT)
     :cljs (.toLowerCase s)))

(defn ascii-digit?
  "Whether the code point `c` is an ASCII digit."
  [^long c]
  (and (<= 0x30 c) (<= c 0x39)))

(defn ascii-hex-digit?
  "Whether the code point `c` is an ASCII hex digit."
  [^long c]
  (or (ascii-digit? c)
      (and (<= 0x41 c) (<= c 0x46))
      (and (<= 0x61 c) (<= c 0x66))))

(defn ascii-alpha?
  "Whether the code point `c` is an ASCII letter."
  [^long c]
  (or (and (<= 0x41 c) (<= c 0x5A))
      (and (<= 0x61 c) (<= c 0x7A))))

(defn ascii-alphanumeric?
  "Whether the code point `c` is an ASCII letter or digit."
  [^long c]
  (or (ascii-alpha? c) (ascii-digit? c)))

;; URL Standard, section 4.3, "URL code points"
(defn url-code-point?
  "Whether the code point `c` is a URL code point: an ASCII letter or
  digit, one of !$&'()*+,-./:;=?@_~, or above U+009F and neither a
  surrogate nor a noncharacter."
  [^long c]
  (if (< c 0x80)
    (or (ascii-alphanumeric? c)
        (case (int c)
          (0x21 0x24 0x26 0x27 0x28 0x29 0x2A 0x2B 0x2C 0x2D 0x2E 0x2F
           0x3A 0x3B 0x3D 0x3F 0x40 0x5F 0x7E) true
          false))
    (and (<= 0xA0 c 0x10FFFD)
         (not (surrogate? c))
         (not (<= 0xFDD0 c 0xFDEF))
         (not (<= 0xFFFE (bit-and c 0xFFFF))))))

(defn hex-value
  "The value of the ASCII hex digit `c`."
  ^long [^long c]
  (cond
    (<= c 0x39) (- c 0x30)
    (<= c 0x46) (- c 0x37)
    :else       (- c 0x57)))

(defn utf-8
  "The bytes of the code point `c` in UTF-8, with U+FFFD for a surrogate."
  [^long c]
  (cond
    (< c 0x80)     [c]
    (< c 0x800)    [(bit-or 0xC0 (bit-shift-right c 6))
                    (bit-or 0x80 (bit-and c 0x3F))]
    (surrogate? c) [0xEF 0xBF 0xBD]
    (< c 0x10000)  [(bit-or 0xE0 (bit-shift-right c 12))
                    (bit-or 0x80 (bit-and (bit-shift-right c 6) 0x3F))
                    (bit-or 0x80 (bit-and c 0x3F))]
    :else          [(bit-or 0xF0 (bit-shift-right c 18))
                    (bit-or 0x80 (bit-and (bit-shift-right c 12) 0x3F))
                    (bit-or 0x80 (bit-and (bit-shift-right c 6) 0x3F))
                    (bit-or 0x80 (bit-and c 0x3F))]))

;; Encoding Standard, 9.1.1, the UTF-8 decoder
(defn utf-8-decode
  "The text of the UTF-8 `bytes`, with U+FFFD for each invalid sequence,
  and with a byte order mark kept as it is."
  [bytes]
  (let [b (builder)]
    (loop [bs     (seq bytes)
           cp     0
           seen   0
           needed 0
           lower  0x80
           upper  0xBF]
      (if-let [[x & more] bs]
        (let [x (long x)]
          (cond
            (and (zero? needed) (<= x 0x7F))
            (do (append-code-point! b x)
                (recur more 0 0 0 0x80 0xBF))

            (and (zero? needed) (<= 0xC2 x 0xDF))
            (recur more (bit-and x 0x1F) 0 1 0x80 0xBF)

            (and (zero? needed) (<= 0xE0 x 0xEF))
            (recur more (bit-and x 0xF) 0 2
                   (if (== x 0xE0) 0xA0 0x80)
                   (if (== x 0xED) 0x9F 0xBF))

            (and (zero? needed) (<= 0xF0 x 0xF4))
            (recur more (bit-and x 0x7) 0 3
                   (if (== x 0xF0) 0x90 0x80)
                   (if (== x 0xF4) 0x8F 0xBF))

            (zero? needed)
            (do (append-code-point! b 0xFFFD)
                (recur more 0 0 0 0x80 0xBF))

            ;; the byte starts over, after the replacement of what came before
            (not (<= lower x upper))
            (do (append-code-point! b 0xFFFD)
                (recur bs 0 0 0 0x80 0xBF))

            :else
            (let [cp   (bit-or (bit-shift-left cp 6) (bit-and x 0x3F))
                  seen (inc seen)]
              (if (== seen needed)
                (do (append-code-point! b cp)
                    (recur more 0 0 0 0x80 0xBF))
                (recur more cp seen needed 0x80 0xBF)))))
        (do (when (pos? needed)
              (append-code-point! b 0xFFFD))
            (str b))))))
