(ns dk.simongray.whatwg-url.hosts
  "The hosts of URLs by the URL Standard, section 3: domains, IPv4 and
  IPv6 addresses, opaque hosts and the empty host.

  A host is the text that parse returns for it, e.g. \"127.0.0.1\" or
  \"[::1]\". The functions that end in -report also return the validation
  errors as keywords. Pass them a vector to add the errors to, or nil to
  skip them."
  (:require [clojure.string :as str]
            [dk.simongray.whatwg-url.chars :as chars]
            [dk.simongray.whatwg-url.idna :as idna]
            [dk.simongray.whatwg-url.percent :as percent]))

(defn ^:no-doc with-error
  "The validation `errors` with the error `k` added, or nil when they
  aren't collected."
  [errors k]
  (some-> errors (conj k)))

;; URL Standard, section 3.2
(defn forbidden-host-code-point?
  "Whether the code point `c` is a forbidden host code point."
  [^long c]
  (case (int c)
    (0x00 0x09 0x0A 0x0D 0x20 0x23 0x2F 0x3A 0x3C 0x3E 0x3F 0x40 0x5B 0x5C
     0x5D 0x5E 0x7C) true
    false))

(defn forbidden-domain-code-point?
  "Whether the code point `c` is a forbidden domain code point."
  [^long c]
  (or (forbidden-host-code-point? c)
      (<= c 0x1F)
      (== c 0x25)
      (== c 0x7F)))

(chars/defscan forbidden-host-code-point-at
  "The index in the text `s` from `i` of its first forbidden host code
  point, or its end."
  [s i] [x] (forbidden-host-code-point? x))

(chars/defscan forbidden-domain-code-point-at
  "The index in the text `s` from `i` of its first forbidden domain code
  point, or its end."
  [s i] [x] (forbidden-domain-code-point? x))

(defn- has?
  "Whether the text `s` has a code unit that `pred` holds for."
  [pred s]
  (let [n (count s)]
    (loop [i 0]
      (cond
        (== i n)                   false
        (pred (chars/code-at s i)) true
        :else                      (recur (inc i))))))

;; URL Standard, section 3.5, the IPv4 number parser
(defn ^:no-doc ipv4-number
  "The number in the part `s` of an IPv4 address and whether it isn't
  decimal, as a pair, or nil when `s` isn't a number. The part can be
  decimal, hexadecimal after 0x or octal after 0. A number above 2^32
  counts as 2^32."
  [s]
  (let [n      (chars/length s)
        zeroed (and (<= 2 n) (== 0x30 (chars/code-at s 0)))
        hex?   (and zeroed (let [x (chars/code-at s 1)] (or (== 0x78 x) (== 0x58 x))))
        ;; ifs rather than a cond, whose last branch would be nil and box
        ;; the number
        radix  (if hex? 16 (if zeroed 8 10))
        start  (if hex? 2 (if zeroed 1 0))]
    (when (pos? n)
      (loop [i start v 0]
        (if (< i n)
          (let [c (chars/code-at s i)]
            (when (and (chars/ascii-hex-digit? c) (< (chars/hex-value c) radix))
              (recur (inc i) (min (+ (* v radix) (chars/hex-value c)) 0x100000000))))
          [v (not= 10 radix)])))))

(defn ends-in-a-number?
  "Whether the last label of the ASCII domain `s` is a number, e.g.
  example.1 or example.0x, which makes the host an IPv4 address. An empty
  label at the end doesn't count."
  [s]
  (let [s    (cond-> s (str/ends-with? s ".") (subs 0 (dec (count s))))
        last (subs s (inc (long (or (str/last-index-of s ".") -1))))]
    ;; a number in any base starts with a digit, which most labels don't
    (boolean (and (seq last)
                  (chars/ascii-digit? (chars/code-at last 0))
                  (or (not (has? #(not (chars/ascii-digit? %)) last))
                      (ipv4-number last))))))

(defn- power-of-256
  "256 to the power of `k`."
  [k]
  (reduce * (repeat k 256)))

(defn- ipv4-parts
  "The parts of the IPv4 address in the ASCII text `s`, without an empty
  last part, and `errors` with the errors of their number added, as a
  pair."
  [s errors]
  (let [parts         (str/split s #"\." -1)
        trailing-dot? (= "" (peek parts))
        parts         (cond-> parts (and trailing-dot? (< 1 (count parts))) pop)]
    [parts (cond-> errors
             trailing-dot?       (with-error :ipv4-empty-part)
             (< (count parts) 4) (with-error :ipv4-too-few-parts))]))

(defn- ipv4-value
  "The IPv4 address of the numbers of its parts, `values`, as a number, or
  nil when they don't fit. The last number fills the bytes that the
  others leave."
  [values]
  (when-not (or (some #(< 255 %) (pop values))
                (<= (power-of-256 (- 5 (count values))) (peek values)))
    (reduce + (peek values)
            (map-indexed #(* %2 (power-of-256 (- 3 %1))) (pop values)))))

;; URL Standard, section 3.5, the IPv4 parser
(defn ipv4-report
  "The IPv4 address in the ASCII text `s` as a number, and `errors` with
  its validation errors added, as a pair. The address is nil when `s`
  isn't one."
  [s errors]
  (let [[parts errors] (ipv4-parts s errors)
        numbers        (map ipv4-number parts)]
    (cond
      (< 4 (count parts))
      [nil (with-error errors :ipv4-too-many-parts)]

      (some nil? numbers)
      [nil (with-error errors :ipv4-non-numeric-part)]

      :else
      (let [values (mapv first numbers)]
        [(ipv4-value values)
         (cond-> errors
           (some second numbers)    (with-error :ipv4-non-decimal-part)
           (some #(< 255 %) values) (with-error :ipv4-out-of-range-part))]))))

(defn ipv4
  "The IPv4 address in the ASCII text `s` as a number, or nil. Its parts
  can be decimal, octal or hexadecimal, and there may be fewer than four.

      (ipv4 \"0x7f.1\")
      ;; => 2130706433"
  [s]
  (first (ipv4-report s nil)))

;; URL Standard, section 3.6
(defn serialize-ipv4
  "The text of the IPv4 address `n`, e.g. 127.0.0.1."
  [n]
  (->> [24 16 8 0]
       (map #(bit-and (bit-shift-right (long n) %) 0xFF))
       (str/join ".")))

;; a function rather than a closure, so that the index isn't boxed
(defn- at
  "The code unit at `i` of the text `s`, or -1 at its end."
  ^long [s ^long i]
  (if (< i (chars/length s)) (chars/code-at s i) -1))

(defn- ipv4-in-ipv6-part
  "The decimal number in the text `s` from `p`, the index after it and the
  error that ends it early, if any, as a triple, as the IPv6 parser reads
  a part of an IPv4 address."
  [s p]
  (loop [value nil p p]
    (if (chars/ascii-digit? (at s p))
      (let [d (- (at s p) 0x30)]
        (cond
          (nil? value)               (recur d (inc p))
          (zero? value)              [nil p :ipv4-in-ipv6-invalid-code-point]
          (< 255 (+ (* value 10) d)) [nil p :ipv4-in-ipv6-out-of-range-part]
          :else                      (recur (+ (* value 10) d) (inc p))))
      [value p nil])))

(defn- ipv4-in-ipv6
  "The `address` with the IPv4 address in the text `s` from `p` set at
  `piece` on, and `errors`, as a pair, or nil and `errors` with the error
  added."
  [address piece s p errors]
  (loop [address address piece piece p p seen 0]
    (if (== -1 (at s p))
      (if (== 4 seen)
        [address errors]
        [nil (with-error errors :ipv4-in-ipv6-too-few-parts)])
      (let [p (if (pos? seen)
                (when (and (== 0x2E (at s p)) (< seen 4)) (inc p))
                p)]
        (if (or (nil? p) (not (chars/ascii-digit? (at s p))))
          [nil (with-error errors :ipv4-in-ipv6-invalid-code-point)]
          (let [[value p failure] (ipv4-in-ipv6-part s p)]
            (if failure
              [nil (with-error errors failure)]
              (let [seen (inc seen)]
                (recur (update address piece #(+ (* % 0x100) value))
                       (cond-> piece (#{2 4} seen) inc)
                       p
                       seen)))))))))

(defn- compressed
  "The `address` with the pieces from `compress` to `piece` moved to its
  end, where the :: of its text put them."
  [address piece compress]
  (loop [address address i 7 swaps (- piece compress)]
    (if (and (pos? i) (pos? swaps))
      (let [j (+ compress swaps -1)]
        (recur (assoc address i (nth address j) j (nth address i))
               (dec i)
               (dec swaps)))
      address)))

(defn- hex-piece
  "The value of the hex digits of the text `s` from `p`, at most four, and
  how many there are, as a pair."
  [s ^long p]
  (loop [value 0 length 0]
    (if (and (< length 4) (chars/ascii-hex-digit? (at s (+ p length))))
      (recur (+ (* value 16) (chars/hex-value (at s (+ p length)))) (inc length))
      [value length])))

(defn- leading-zero?
  "Whether the piece `value`, written with `length` hex digits, starts
  with a zero that it doesn't need."
  [^long value ^long length]
  (and (< 1 length) (< value (bit-shift-left 1 (* 4 (dec length))))))

(defn- finished
  "The `address` and `errors` as a pair, for a text that ends after `piece`
  pieces, with the :: of `compress` if any, or nil and `errors` with the
  error when the pieces are too few."
  [address piece compress errors]
  (cond
    compress     [(compressed address piece compress) errors]
    (== 8 piece) [address errors]
    :else        [nil (with-error errors :ipv6-too-few-pieces)]))

(defn- ipv4-tail
  "The `address` with the IPv4 address that ends the text `s` from `p` set
  at `piece` on, after `length` hex digits, with the :: of `compress` if
  any, and `errors`, as a pair, or nil and `errors` with the error added."
  [address piece compress s p length errors]
  (cond
    (zero? length) [nil (with-error errors :ipv4-in-ipv6-invalid-code-point)]
    (< 6 piece)    [nil (with-error errors :ipv4-in-ipv6-too-many-pieces)]
    :else          (let [[address errors] (ipv4-in-ipv6 address piece s p errors)]
                     (if address
                       (finished address (+ piece 2) compress errors)
                       [nil errors]))))

;; URL Standard, section 3.5, the IPv6 parser
(defn ipv6-report
  "The IPv6 address in the text `s` (without brackets) as its eight 16-bit
  pieces, and `errors` with its validation errors added, as a pair. The
  address is nil when `s` isn't one."
  [s errors]
  (let [compressed? (== 0x3A (at s 0))]
    (if (and compressed? (not= 0x3A (at s 1)))
      [nil (with-error errors :ipv6-invalid-compression)]
      (loop [address  [0 0 0 0 0 0 0 0]
             p        (if compressed? 2 0)
             piece    (if compressed? 1 0)
             compress (when compressed? 1)
             errors   errors]
        (cond
          (== -1 (at s p))   (finished address piece compress errors)
          (== 8 piece)       [nil (with-error errors :ipv6-too-many-pieces)]
          (== 0x3A (at s p)) (if compress
                               [nil (with-error errors :ipv6-multiple-compression)]
                               (recur address (inc p) (inc piece) (inc piece) errors))
          :else
          (let [[value length] (hex-piece s p)
                q              (+ p (long length))
                x              (at s q)
                errors         (cond-> errors
                                 (leading-zero? value length) (with-error :ipv6-piece-leading-zero))]
            (cond
              (== 0x2E x)
              (ipv4-tail address piece compress s p length errors)

              (or (and (== 0x3A x) (== -1 (at s (inc q))))
                  (and (not= 0x3A x) (not= -1 x)))
              [nil (with-error errors :ipv6-invalid-code-point)]

              :else
              (recur (assoc address piece value)
                     (cond-> q (== 0x3A x) inc)
                     (inc piece)
                     compress
                     errors))))))))

(defn ipv6
  "The eight 16-bit pieces of the IPv6 address in the text `s` (without
  brackets), or nil.

      (ipv6 \"::1\")
      ;; => [0 0 0 0 0 0 0 1]"
  [s]
  (first (ipv6-report s nil)))

;; URL Standard, section 3.6, finding the IPv6 address compressed piece
;; index, in one pass over the pieces
(defn- compressed-index
  "The index of the first of the longest run of more than one zero piece
  of the IPv6 `address`, or nil."
  [address]
  (loop [i 0 start -1 best -1 best-length 1]
    (cond
      (== 8 i)                         (when-not (neg? best) best)
      (pos? (long (nth address i)))    (recur (inc i) -1 best best-length)
      :else                            (let [start  (if (neg? start) i start)
                                             length (- (inc i) start)]
                                         (if (< best-length length)
                                           (recur (inc i) start start length)
                                           (recur (inc i) start best best-length))))))

;; URL Standard, section 3.6, the IPv6 serializer, into one builder
(defn serialize-ipv6
  "The text of the IPv6 address of the eight 16-bit `pieces`, without
  brackets, e.g. 2001:db8::1."
  [pieces]
  (let [compress (compressed-index pieces)
        b        (chars/builder)]
    (loop [i 0 ignore-0? false]
      (when (< i 8)
        (let [piece (long (nth pieces i))]
          (cond
            (and ignore-0? (zero? piece)) (recur (inc i) true)
            (= compress i)                (do (chars/append! b (if (zero? i) "::" ":"))
                                              (recur (inc i) true))
            :else                         (do (chars/append! b #?(:clj  (Long/toHexString piece)
                                                                  :cljs (.toString piece 16)))
                                              (when (< i 7) (chars/append! b ":"))
                                              (recur (inc i) false))))))
    (str b)))

;; URL Standard, section 3.5, the opaque-host parser
(defn- opaque-report
  "The opaque host of the text `s`, and `errors` with its validation
  errors added, as a pair."
  [s errors]
  (if (< (forbidden-host-code-point-at s 0) (chars/length s))
    [nil (with-error errors :host-invalid-code-point)]
    [(percent/encode s percent/c0-control-set)
     (cond-> errors
       (not (percent/url-units? s 0 (count s))) (with-error :invalid-url-unit))]))

(def strict-options
  "The options of idna/to-ascii that the URL Standard's domain parser uses
  when it's strict."
  (assoc idna/default-options
         :check-hyphens?        true
         :use-std3-ascii-rules? true
         :verify-dns-length?    true))

;; URL Standard, section 3.3, the domain parser
(defn domain-report
  "The Unicode `domain` in ASCII by IDNA, and `errors` with its validation
  errors added, as a pair. The domain is nil when it doesn't convert. When
  `strict?`, it must also be a domain that DNS allows."
  [domain strict? errors]
  (let [strict-result (when (or strict? errors) (idna/to-ascii domain strict-options))
        errors        (cond-> errors (nil? strict-result) (with-error :domain-to-ascii))
        result        (cond
                        strict?               strict-result
                        (chars/ascii? domain) (chars/lower-case domain)
                        :else                 (idna/to-ascii domain))]
    [(when (and (seq result)
                (== (chars/length result) (forbidden-domain-code-point-at result 0)))
       result)
     errors]))

(defn domain->ascii
  "The Unicode `domain` in ASCII by the domain parser of the URL Standard,
  or nil when it doesn't convert, with `opts`:

  - :strict?, true to only allow the domains that DNS allows, e.g. with no
    _ and no label longer than 63

      (domain->ascii \"Bücher.example\")
      ;; => \"xn--bcher-kva.example\""
  ([domain]
   (domain->ascii domain {}))
  ([domain {:keys [strict?] :as opts}]
   (first (domain-report domain (boolean strict?) nil))))

;; URL Standard, section 3.3
(defn domain->unicode
  "The `domain` in Unicode, for showing it to a person, or as it is when it
  doesn't convert.

      (domain->unicode \"xn--bcher-kva.example\")
      ;; => \"bücher.example\""
  [domain]
  (or (idna/to-unicode domain) domain))

;; URL Standard, section 3.4
(defn valid-domain?
  "Whether the text `s` is a valid domain: one that the strict domain
  parser converts, and whose last label isn't a number."
  [s]
  (let [domain (domain->ascii s {:strict? true})]
    (boolean (and domain (not (ends-in-a-number? domain))))))

(defn- bracketed-report
  "The IPv6 host of the text `s`, which starts with [, and `errors` with
  its validation errors added, as a pair."
  [s errors]
  (if (str/ends-with? s "]")
    (let [[pieces errors] (ipv6-report (subs s 1 (dec (count s))) errors)]
      [(some->> pieces serialize-ipv6 (#(str "[" % "]"))) errors])
    [nil (with-error errors :ipv6-unclosed)]))

(defn- percent-encoded-error
  "The `errors` with :domain-percent-encoded added when the host text `s`
  has a percent-encoded byte, if errors are collected."
  [errors s]
  (cond-> errors
    (and errors
         (str/includes? s "%")
         (some #(percent/percent-encoded-byte-at? s %) (range (count s))))
    (with-error :domain-percent-encoded)))

(defn- domain-host-report
  "The domain or IPv4 address that the host text `s` of a special URL
  names, and `errors` with its validation errors added, as a pair."
  [s errors]
  (let [errors         (percent-encoded-error errors s)
        domain         (percent/decode s)
        [ascii errors] (domain-report domain false errors)]
    (cond
      (nil? ascii)
      [nil errors]

      (ends-in-a-number? ascii)
      (let [errors     (cond-> errors
                         (not (chars/ascii? domain)) (with-error :ipv4-non-ascii-input))
            [n errors] (ipv4-report ascii errors)]
        [(some-> n serialize-ipv4) errors])

      :else
      [ascii errors])))

;; URL Standard, section 3.5, the host parser
(defn parse-report
  "The host of the text `s`, opaque when `opaque?`, and `errors` with its
  validation errors added, as a pair. The host is nil when it doesn't
  parse."
  [s opaque? errors]
  (cond
    (str/starts-with? s "[") (bracketed-report s errors)
    opaque?                  (opaque-report s errors)
    :else                    (domain-host-report s errors)))

(defn parse
  "The host in the text `s`, parsed as the host of a URL and returned as
  text, or nil when it doesn't parse. With `opts`:

  - :opaque?, true for the host of a URL whose scheme isn't special, which
    is only percent-encoded

      (parse \"EXAMPLE.com\")      ;; => \"example.com\"
      (parse \"0x7f.1\")           ;; => \"127.0.0.1\"
      (parse \"[0:0::1]\")         ;; => \"[::1]\"
      (parse \"faß.example\")      ;; => \"xn--fa-hia.example\""
  ([s]
   (parse s {}))
  ([s {:keys [opaque?] :as opts}]
   (first (parse-report s (boolean opaque?) nil))))

(defn suffix-rules
  "The rules of the Public Suffix List in its text `s` (e.g. that of
  public_suffix_list.dat from https://publicsuffix.org/list/), for
  public-suffix and registrable-domain. They're a map of the :rules and the
  :exceptions (without their !), all in ASCII."
  [s]
  (reduce (fn [rules line]
            (let [rule (first (str/split (str/trim line) #"\s+"))]
              (cond
                (or (= "" rule) (str/starts-with? rule "//"))
                rules

                (str/starts-with? rule "!")
                (update rules :exceptions conj (or (domain->ascii (subs rule 1)) (subs rule 1)))

                :else
                (update rules :rules conj (or (domain->ascii rule) rule)))))
          {:rules #{} :exceptions #{}}
          (str/split-lines s)))

;; https://github.com/publicsuffix/list/wiki/Format, "Algorithm"
(defn- suffix-length
  "The number of the `labels` of a domain that its public suffix has by the
  `rules`."
  [labels {:keys [rules exceptions]}]
  (let [suffix   #(str/join "." (take-last % labels))
        wildcard #(str/join "." (cons "*" (take-last (dec %) labels)))
        lengths  (range (count labels) 0 -1)]
    (or (some #(when (contains? exceptions (suffix %)) (dec %)) lengths)
        (some #(when (or (contains? rules (suffix %)) (contains? rules (wildcard %))) %)
              lengths)
        1)))

(defn- suffix-labels
  "The labels of the domain `host` without its dot at the end, and the
  number of them its public suffix has by the `rules`, and whether it ends
  in a dot, or nil when the host isn't a domain or has an empty label, as
  the tests of the list have it."
  [host rules]
  (let [dot?   (str/ends-with? host ".")
        domain (cond-> host dot? (subs 0 (dec (count host))))
        labels (str/split domain #"\." -1)]
    (when-not (or (str/starts-with? host "[")
                  (some #(= "" %) labels)
                  (ends-in-a-number? host))
      [labels (suffix-length labels rules) dot?])))

;; URL Standard, section 3.2
(defn public-suffix
  "The public suffix of the domain `host` by the `rules` of suffix-rules,
  e.g. com or github.io, or nil when the host isn't a domain. When no rule
  matches, it's the last label of the host.

      (public-suffix \"whatwg.github.io\" rules)
      ;; => \"github.io\""
  [host rules]
  (when-let [[labels n dot?] (suffix-labels host rules)]
    (str (str/join "." (take-last n labels)) (when dot? "."))))

;; URL Standard, section 3.2
(defn registrable-domain
  "The registrable domain of the domain `host` by the `rules` of
  suffix-rules: its public suffix and the label before it, e.g.
  example.com for www.example.com, or nil when there's no label before it.

      (registrable-domain \"sub.www.example.com\" rules)
      ;; => \"example.com\""
  [host rules]
  (when-let [[labels n dot?] (suffix-labels host rules)]
    (let [domain (take-last (inc n) labels)]
      (when (and (< n (count labels)) (not= "" (first domain)))
        (str (str/join "." domain) (when dot? "."))))))

(comment
  (parse "EXAMPLE.COM")
  (parse "EXAMPLE.COM" {:opaque? true})
  (parse "faß.example" {:opaque? true})
  (parse "0xffffffff")
  (parse "09")
  (parse-report "https://127.0.0x0.1" false [])
  (parse-report "[::01]" false [])
  #_.)
