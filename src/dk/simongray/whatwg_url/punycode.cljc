(ns dk.simongray.whatwg-url.punycode
  "Punycode, the encoding of Unicode labels of domain names in ASCII, by
  RFC 3492, as in the xn--fa-hia of xn--fa-hia.de, which is faß.de.

  The numbers are limited to 31 bits, as most implementations do."
  (:require [dk.simongray.whatwg-url.chars :as chars]))

;; RFC 3492, section 5
(def ^:no-doc base
  "The number of Punycode digits."
  36)

(def ^:no-doc tmin
  "The least threshold of a digit."
  1)

(def ^:no-doc tmax
  "The greatest threshold of a digit."
  26)

(def ^:no-doc max-int
  "The greatest number that encoding and decoding allow."
  0x7FFFFFFF)

;; RFC 3492, section 6.1
(defn- adapt
  "The bias after the `delta` of a code point, of `n` code points so far,
  when it's the `first?`."
  [delta n first?]
  (loop [delta (let [d (quot delta (if first? 700 2))]
                 (+ d (quot d n)))
         k     0]
    (if (< (quot (* (- base tmin) tmax) 2) delta)
      (recur (quot delta (- base tmin)) (+ k base))
      (+ k (quot (* (inc (- base tmin)) delta) (+ delta 38))))))

(defn- threshold
  "The threshold of the digit at `k` with `bias`."
  [k bias]
  (cond
    (<= k bias)           tmin
    (<= (+ bias tmax) k)  tmax
    :else                 (- k bias)))

(defn- digit-value
  "The value of the Punycode digit `c`, or base for a code point that's no
  digit."
  [c]
  (cond
    (<= 0x30 c 0x39) (- c 22)
    (<= 0x41 c 0x5A) (- c 0x41)
    (<= 0x61 c 0x7A) (- c 0x61)
    :else            base))

(defn- digit
  "The lower-case Punycode digit of the value `d`."
  [d]
  (if (< d 26) (+ d 0x61) (+ d 22)))

(defn- insertion
  "The insertion that the digits of `cps` from `in` give after the insertion
  `i` with `bias`, as the next index and the new insertion, or nil when
  they run out or overflow."
  [cps in i bias]
  (loop [in in i i w 1 k base]
    (when (< in (count cps))
      (let [d (digit-value (nth cps in))]
        (when (and (< d base) (<= d (quot (- max-int i) w)))
          (let [i (+ i (* d w))
                t (threshold k bias)]
            (cond
              (< d t)                         [(inc in) i]
              (< (quot max-int (- base t)) w) nil
              :else                           (recur (inc in) i (* w (- base t)) (+ k base)))))))))

;; RFC 3492, section 6.2
(defn decode
  "The Unicode text of the Punycode `s`, without its xn--, or nil when it
  isn't Punycode.

      (decode \"fa-hia\")
      ;; => \"faß\""
  [s]
  (let [cps   (chars/code-points s)
        delim (or (last (keep-indexed #(when (== 0x2D %2) %1) cps)) 0)
        basic (subvec cps 0 delim)]
    (when (every? #(< % 0x80) basic)
      (loop [out  basic
             in   (if (pos? delim) (inc delim) 0)
             n    0x80
             i    0
             bias 72]
        (if (< in (count cps))
          (when-let [[in' i'] (insertion cps in i bias)]
            (let [len  (inc (count out))
                  bias (adapt (- i' i) len (zero? i))
                  n    (+ n (quot i' len))
                  i    (rem i' len)]
              (when (and (<= n 0x10FFFF) (not (chars/surrogate? n)))
                (recur (into (conj (subvec out 0 i) n) (subvec out i))
                       in' n (inc i) bias))))
          (chars/from-code-points out))))))

(defn- encode-delta
  "The digits of `out` with the `delta` of a code point appended, by
  `bias`."
  [out delta bias]
  (loop [out out q delta k base]
    (let [t (threshold k bias)]
      (if (< q t)
        (conj out (digit q))
        (recur (conj out (digit (+ t (rem (- q t) (- base t)))))
               (quot (- q t) (- base t))
               (+ k base))))))

;; RFC 3492, section 6.3
(defn encode
  "The Punycode of the Unicode text `s`, without an xn--, or nil when it's
  too long to encode.

      (encode \"faß\")
      ;; => \"fa-hia\""
  [s]
  (let [cps   (chars/code-points s)
        basic (filterv #(< % 0x80) cps)
        b     (count basic)]
    (loop [out   (cond-> basic (pos? b) (conj 0x2D))
           n     0x80
           delta 0
           bias  72
           h     b]
      (if (< h (count cps))
        (let [m     (reduce min (filter #(<= n %) cps))
              delta (+ delta (* (- m n) (inc h)))]
          (when (<= delta max-int)
            (let [[out delta bias h]
                  (reduce (fn [[out delta bias h] c]
                            (cond
                              (< c m)  [out (inc delta) bias h]
                              (== c m) [(encode-delta out delta bias)
                                        0
                                        (adapt delta (inc h) (== h b))
                                        (inc h)]
                              :else    [out delta bias h]))
                          [out delta bias h]
                          cps)]
              (when (<= delta max-int)
                (recur out (inc m) (inc delta) bias h)))))
        (chars/from-code-points out)))))

(comment
  (encode "bücher")
  (decode "bcher-kva")
  (decode "egbpdaj6bu4bxfgehfvwxn")
  #_.)
