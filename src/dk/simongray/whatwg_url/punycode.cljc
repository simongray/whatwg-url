(ns dk.simongray.whatwg-url.punycode
  "Punycode by RFC 3492, which writes a Unicode label of a domain name in
  ASCII, e.g. the xn--fa-hia of xn--fa-hia.de for faß.de.

  Like most implementations, it limits its numbers to 31 bits."
  (:require [dk.simongray.whatwg-url.chars :as chars]))

;; RFC 3492, section 5, as constants, which the compiler inlines as
;; primitive numbers
(def ^:no-doc ^:const base
  "The number of Punycode digits."
  36)

(def ^:no-doc ^:const tmin
  "The least threshold of a digit."
  1)

(def ^:no-doc ^:const tmax
  "The greatest threshold of a digit."
  26)

(def ^:no-doc ^:const initial-n
  "The code point that encoding and decoding start from."
  0x80)

(def ^:no-doc ^:const initial-bias
  "The bias that encoding and decoding start with."
  72)

(def ^:no-doc ^:const max-int
  "The greatest number that encoding and decoding allow."
  0x7FFFFFFF)

;; RFC 3492, section 6.1
(defn- adapt
  "The bias after the `delta` of a code point, of `n` code points so far,
  when it's the `first?`."
  ^long [^long delta ^long n first?]
  (loop [delta (let [d (quot delta (if first? 700 2))]
                 (+ d (quot d n)))
         k     0]
    (if (< (quot (* (- base tmin) tmax) 2) delta)
      (recur (quot delta (- base tmin)) (+ k base))
      (+ k (quot (* (inc (- base tmin)) delta) (+ delta 38))))))

(defn- threshold
  "The threshold of the digit at `k` with `bias`."
  ^long [^long k ^long bias]
  (cond
    (<= k bias)          tmin
    (<= (+ bias tmax) k) tmax
    :else                (- k bias)))

;; two comparisons for each range, as only those of two numbers are inlined
(defn- digit-value
  "The value of the Punycode digit `c`, or base for a code point that's no
  digit."
  ^long [^long c]
  (cond
    (and (<= 0x30 c) (<= c 0x39)) (- c 22)
    (and (<= 0x41 c) (<= c 0x5A)) (- c 0x41)
    (and (<= 0x61 c) (<= c 0x7A)) (- c 0x61)
    :else                         base))

(defn- digit
  "The lower-case Punycode digit of the value `d`."
  ^long [^long d]
  (if (< d 26) (+ d 0x61) (+ d 22)))

(defn- insertion
  "The insertion that the digits of `cps` from `in` give after the insertion
  `i` with `bias`, as the next index and the new insertion, or nil when
  they run out or overflow."
  [cps ^long in ^long i ^long bias]
  (loop [in in i i w 1 k base]
    (when (< in (count cps))
      (let [d (digit-value (nth cps in))]
        (when (and (< d base) (<= d (quot (- max-int i) w)))
          (let [i (+ i (* d w))
                t (threshold k bias)]
            (cond
              (< d t)                         [(inc in) i]
              (< (quot max-int (- base t)) w) nil
              :else                           (recur (inc in) i
                                                     (* w (- base t))
                                                     (+ k base)))))))))

;; RFC 3492, section 6.2
(defn decode
  "The Unicode text of the Punycode `s`, without its xn--, or nil when it
  isn't Punycode.

      (decode \"fa-hia\")
      ;; => \"faß\""
  [s]
  (let [cps   (chars/code-points s)
        delim (long (or (last (keep-indexed #(when (== 0x2D (long %2)) %1) cps)) 0))
        basic (subvec cps 0 delim)]
    (when (every? #(< (long %) 0x80) basic)
      (loop [out  basic
             in   (if (pos? delim) (inc delim) 0)
             n    initial-n
             i    0
             bias initial-bias]
        (if (< in (count cps))
          (when-let [[in' i'] (insertion cps in i bias)]
            (let [in'  (long in')
                  i'   (long i')
                  len  (inc (count out))
                  bias (adapt (- i' i) len (zero? i))
                  n    (+ n (quot i' len))
                  i    (rem i' len)]
              (when (and (<= n 0x10FFFF) (not (chars/surrogate? n)))
                (recur (into (conj (subvec out 0 i) n) (subvec out i))
                       in' n (inc i) bias))))
          (chars/from-code-points out))))))

(defn- encode-delta
  "The text builder `out` with the digits of the `delta` of a code point
  appended, by `bias`."
  [out ^long delta ^long bias]
  (loop [q delta k base]
    (let [t (threshold k bias)]
      (if (< q t)
        (chars/append-code-point! out (digit q))
        (do (chars/append-code-point! out (digit (+ t (rem (- q t) (- base t)))))
            (recur (quot (- q t) (- base t)) (+ k base)))))))

;; a function of its own, since areduce inside an expression would be
;; compiled to a closure that boxes the count
(defn- basic-count
  "The number of the code points `cps` that are basic, i.e. ASCII."
  ^long [^longs cps]
  (areduce cps k basic 0 (if (< (aget cps k) 0x80) (inc basic) basic)))

(defn- least-from
  "The least of the code points `cps` that isn't less than `n`, or one
  past the last code point when there's none."
  ^long [^longs cps ^long n]
  (areduce cps k m 0x110000
           (let [c (aget cps k)]
             (if (and (<= n c) (< c m)) c m))))

;; one vector a round, for the few code points that aren't ASCII, rather
;; than one for each code point
(defn- encode-round
  "The delta, the bias and the number of code points handled after the
  round of the code point `m` over the code points `cps`, of which `b`
  are basic, as a vector, with the digits of each delta appended to
  `out`."
  [^longs cps m delta bias h b out]
  (let [m (long m) b (long b)]
    (loop [k 0 delta (long delta) bias (long bias) h (long h)]
      (if (< k (alength cps))
        (let [c (aget cps k)]
          (cond
            (< c m)  (recur (inc k) (inc delta) bias h)
            (== c m) (do (encode-delta out delta bias)
                         (recur (inc k) 0 (adapt delta (inc h) (== h b)) (inc h)))
            :else    (recur (inc k) delta bias h)))
        [delta bias h]))))

;; RFC 3492, section 6.3, with the code points in an array of primitive
;; numbers
(defn encode
  "The Punycode of the Unicode text `s`, without an xn--, or nil when it's
  too long to encode.

      (encode \"faß\")
      ;; => \"fa-hia\""
  [s]
  (let [cps (long-array (chars/code-points s))
        out (chars/builder)
        b   (basic-count cps)]
    (dotimes [k (alength cps)]
      (when (< (aget cps k) 0x80)
        (chars/append-code-point! out (aget cps k))))
    (when (pos? b)
      (chars/append! out "-"))
    (loop [n initial-n delta 0 bias initial-bias h b]
      (if (< h (alength cps))
        (let [m     (least-from cps n)
              delta (+ delta (* (- m n) (inc h)))]
          (when (<= delta max-int)
            (let [[delta bias h] (encode-round cps m delta bias h b out)]
              (when (<= (long delta) max-int)
                (recur (inc m) (inc (long delta)) (long bias) (long h))))))
        (str out)))))

(comment
  (encode "bücher")
  (decode "bcher-kva")
  (decode "egbpdaj6bu4bxfgehfvwxn")
  #_.)
