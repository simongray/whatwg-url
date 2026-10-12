(ns dk.simongray.whatwg-url.hosts
  "Hosts: domains, IPv4 and IPv6 addresses, opaque hosts and the empty
  host, by the URL Standard, section 3.

  A host is the text of its serialization, e.g. \"127.0.0.1\" or
  \"[::1]\", as parse gives it. The functions that end in -report also give
  the validation errors, as keywords: give them a vector to add the errors
  to, or nil to leave them out."
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
  "The number that the part `s` of an IPv4 address writes, in decimal,
  hexadecimal after 0x or octal after 0, and whether it isn't decimal, as a
  pair, or nil when it doesn't read. A number above 2^32 is 2^32."
  [s]
  (let [n             (count s)
        [radix start] (cond
                        (and (<= 2 n) (#{"0x" "0X"} (subs s 0 2))) [16 2]
                        (and (<= 2 n) (= "0" (subs s 0 1)))        [8 1]
                        :else                                      [10 0])
        digit         (fn [c]
                        (when (chars/ascii-hex-digit? c)
                          (let [v (chars/hex-value c)]
                            (when (< v radix) v))))]
    (when (pos? n)
      (loop [i start v 0]
        (if (< i n)
          (when-let [d (digit (chars/code-at s i))]
            (recur (inc i) (min (+ (* v radix) d) 0x100000000)))
          [v (not= 10 radix)])))))

(defn ends-in-a-number?
  "Whether the last label of the ASCII domain `s`, not counting an empty
  one at its end, is a number, so that it's read as an IPv4 address, e.g.
  example.1 or example.0x."
  [s]
  (let [s    (cond-> s (str/ends-with? s ".") (subs 0 (dec (count s))))
        last (subs s (inc (or (str/last-index-of s ".") -1)))]
    (boolean (or (and (seq last) (not (has? #(not (chars/ascii-digit? %)) last)))
                 (ipv4-number last)))))

(defn- power-of-256
  "256 to the power of `k`."
  [k]
  (reduce * (repeat k 256)))

;; URL Standard, section 3.5, the IPv4 parser
(defn ipv4-report
  "The IPv4 address that the ASCII text `s` writes as a number, and
  `errors` with its validation errors added, as a pair. The address is nil
  when it doesn't read."
  [s errors]
  (let [parts         (str/split s #"\." -1)
        trailing-dot? (= "" (peek parts))
        errors        (cond-> errors trailing-dot? (with-error :ipv4-empty-part))
        parts         (cond-> parts (and trailing-dot? (< 1 (count parts))) pop)
        errors        (cond-> errors (< (count parts) 4) (with-error :ipv4-too-few-parts))
        numbers       (map ipv4-number parts)]
    (cond
      (< 4 (count parts))
      [nil (with-error errors :ipv4-too-many-parts)]

      (some nil? numbers)
      [nil (with-error errors :ipv4-non-numeric-part)]

      :else
      (let [values (mapv first numbers)
            errors (cond-> errors
                     (some second numbers)    (with-error :ipv4-non-decimal-part)
                     (some #(< 255 %) values) (with-error :ipv4-out-of-range-part))]
        (if (or (some #(< 255 %) (pop values))
                (<= (power-of-256 (- 5 (count values))) (peek values)))
          [nil errors]
          [(reduce + (peek values)
                   (map-indexed #(* %2 (power-of-256 (- 3 %1))) (pop values)))
           errors])))))

(defn ipv4
  "The IPv4 address that the ASCII text `s` writes, as a number, or nil.
  Each part can be written in decimal, octal or hexadecimal, and there can
  be fewer than four.

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

(defn- ipv4-in-ipv6
  "The `address` with the IPv4 address in the text `s` from `p` set at
  `piece` on, and `errors`, as a pair, or nil and `errors` with the error
  added."
  [address piece s p errors]
  (let [n (count s)
        c #(if (< % n) (chars/code-at s %) -1)]
    (loop [address address piece piece p p seen 0]
      (if (== -1 (c p))
        (if (== 4 seen)
          [address errors]
          [nil (with-error errors :ipv4-in-ipv6-too-few-parts)])
        (let [p (if (pos? seen)
                  (when (and (== 0x2E (c p)) (< seen 4)) (inc p))
                  p)]
          (if (or (nil? p) (not (chars/ascii-digit? (c p))))
            [nil (with-error errors :ipv4-in-ipv6-invalid-code-point)]
            (let [[value p failure]
                  (loop [value nil p p]
                    (if (chars/ascii-digit? (c p))
                      (let [d (- (c p) 0x30)]
                        (cond
                          (nil? value)  (recur d (inc p))
                          (zero? value) [nil p :ipv4-in-ipv6-invalid-code-point]
                          (< 255 (+ (* value 10) d))
                          [nil p :ipv4-in-ipv6-out-of-range-part]
                          :else         (recur (+ (* value 10) d) (inc p))))
                      [value p nil]))]
              (if failure
                [nil (with-error errors failure)]
                (let [seen (inc seen)]
                  (recur (update address piece #(+ (* % 0x100) value))
                         (cond-> piece (#{2 4} seen) inc)
                         p
                         seen))))))))))

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

;; URL Standard, section 3.5, the IPv6 parser
(defn ipv6-report
  "The IPv6 address that the text `s` writes without its brackets, as its
  eight 16-bit pieces, and `errors` with its validation errors added, as a
  pair. The address is nil when it doesn't read."
  [s errors]
  (let [n (count s)
        c #(if (< % n) (chars/code-at s %) -1)]
    (if (and (== 0x3A (c 0)) (not= 0x3A (c 1)))
      [nil (with-error errors :ipv6-invalid-compression)]
      (loop [address  [0 0 0 0 0 0 0 0]
             p        (if (== 0x3A (c 0)) 2 0)
             piece    (if (== 0x3A (c 0)) 1 0)
             compress (when (== 0x3A (c 0)) 1)
             errors   errors]
        (cond
          (== -1 (c p))
          (cond
            compress     [(compressed address piece compress) errors]
            (== 8 piece) [address errors]
            :else        [nil (with-error errors :ipv6-too-few-pieces)])

          (== 8 piece)
          [nil (with-error errors :ipv6-too-many-pieces)]

          (== 0x3A (c p))
          (if compress
            [nil (with-error errors :ipv6-multiple-compression)]
            (recur address (inc p) (inc piece) (inc piece) errors))

          :else
          (let [[value length]
                (loop [value 0 length 0]
                  (if (and (< length 4) (chars/ascii-hex-digit? (c (+ p length))))
                    (recur (+ (* value 16) (chars/hex-value (c (+ p length)))) (inc length))
                    [value length]))
                q      (+ p length)
                errors (cond-> errors
                         (and (< 1 length) (< value (bit-shift-left 1 (* 4 (dec length)))))
                         (with-error :ipv6-piece-leading-zero))]
            (cond
              (== 0x2E (c q))
              (cond
                (zero? length)
                [nil (with-error errors :ipv4-in-ipv6-invalid-code-point)]

                (< 6 piece)
                [nil (with-error errors :ipv4-in-ipv6-too-many-pieces)]

                :else
                (let [[address errors] (ipv4-in-ipv6 address piece s p errors)]
                  (cond
                    (nil? address) [nil errors]
                    compress       [(compressed address (+ piece 2) compress) errors]
                    (== 6 piece)   [address errors]
                    :else          [nil (with-error errors :ipv6-too-few-pieces)])))

              (and (== 0x3A (c q)) (== -1 (c (inc q))))
              [nil (with-error errors :ipv6-invalid-code-point)]

              (and (not= 0x3A (c q)) (not= -1 (c q)))
              [nil (with-error errors :ipv6-invalid-code-point)]

              :else
              (recur (assoc address piece value)
                     (cond-> q (== 0x3A (c q)) inc)
                     (inc piece)
                     compress
                     errors))))))))

(defn ipv6
  "The eight 16-bit pieces of the IPv6 address that the text `s` writes
  without its brackets, or nil.

      (ipv6 \"::1\")
      ;; => [0 0 0 0 0 0 0 1]"
  [s]
  (first (ipv6-report s nil)))

;; URL Standard, section 3.6, finding the IPv6 address compressed piece index
(defn- compressed-index
  "The index of the first of the longest run of more than one zero piece
  of the IPv6 `address`, or nil."
  [address]
  (let [runs (->> (map-indexed vector address)
                  (partition-by #(zero? (second %)))
                  (filter #(zero? (second (first %))))
                  (filter #(< 1 (count %))))]
    (when (seq runs)
      (first (first (reduce #(if (< (count %1) (count %2)) %2 %1) runs))))))

(defn serialize-ipv6
  "The text of the IPv6 address of the eight 16-bit `pieces`, without
  brackets, e.g. 2001:db8::1."
  [pieces]
  (let [compress (compressed-index pieces)
        hex      #?(:clj #(Long/toHexString %) :cljs #(.toString % 16))]
    (if compress
      (let [end (loop [i compress]
                  (if (and (< i 8) (zero? (nth pieces i))) (recur (inc i)) i))]
        (str (str/join ":" (map hex (subvec pieces 0 compress)))
             "::"
             (str/join ":" (map hex (subvec pieces end)))))
      (str/join ":" (map hex pieces)))))

;; URL Standard, section 3.5, the opaque-host parser
(defn- opaque-report
  "The opaque host of the text `s`, and `errors` with its validation
  errors added, as a pair."
  [s errors]
  (if (has? forbidden-host-code-point? s)
    [nil (with-error errors :host-invalid-code-point)]
    [(percent/encode s percent/c0-control-set)
     (cond-> errors
       (not (percent/url-units? s 0 (count s))) (with-error :invalid-url-unit))]))

(def strict-options
  "The flags of UTS #46 that the URL Standard's domain parser gives
  Unicode ToASCII when it's strict."
  (assoc idna/default-options
         :check-hyphens?        true
         :use-std3-ascii-rules? true
         :verify-dns-length?    true))

;; URL Standard, section 3.3, the domain parser
(defn domain-report
  "The ASCII domain of the Unicode `domain`, by IDNA, and `errors` with its
  validation errors added, as a pair. The domain is nil when it doesn't
  convert, and when `strict?`, it must also be one that DNS allows."
  [domain strict? errors]
  (let [strict-result (when (or strict? errors) (idna/to-ascii domain strict-options))
        errors        (cond-> errors (nil? strict-result) (with-error :domain-to-ascii))
        result        (cond
                        strict?               strict-result
                        (chars/ascii? domain) (chars/lower-case domain)
                        :else                 (idna/to-ascii domain))]
    [(when (and (seq result) (not (has? forbidden-domain-code-point? result)))
       result)
     errors]))

(defn domain->ascii
  "The ASCII domain of the Unicode `domain` by the domain parser of the URL
  Standard, or nil, with `opts`:

  - :strict?, true to also allow only domains that DNS allows, e.g. with
    no _ and no label longer than 63

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

;; URL Standard, section 3.5, the host parser
(defn parse-report
  "The host of the text `s`, opaque when `opaque?`, and `errors` with its
  validation errors added, as a pair. The host is nil when it doesn't
  parse."
  [s opaque? errors]
  (cond
    (str/starts-with? s "[")
    (if (str/ends-with? s "]")
      (let [[pieces errors] (ipv6-report (subs s 1 (dec (count s))) errors)]
        [(some->> pieces serialize-ipv6 (#(str "[" % "]"))) errors])
      [nil (with-error errors :ipv6-unclosed)])

    opaque?
    (opaque-report s errors)

    :else
    (let [errors         (cond-> errors
                           (and errors
                                (str/includes? s "%")
                                (some #(percent/percent-encoded-byte-at? s %) (range (count s))))
                           (with-error :domain-percent-encoded))
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
        [ascii errors]))))

(defn parse
  "The host of the text `s` as the URL Standard parses the host of a URL,
  as its serialization, or nil when it doesn't parse. With `opts`:

  - :opaque?, true for the host of a URL whose scheme isn't special, which
    is kept as it is, percent-encoded

      (parse \"EXAMPLE.com\")      ;; => \"example.com\"
      (parse \"0x7f.1\")           ;; => \"127.0.0.1\"
      (parse \"[0:0::1]\")         ;; => \"[::1]\"
      (parse \"faß.example\")      ;; => \"xn--fa-hia.example\""
  ([s]
   (parse s {}))
  ([s {:keys [opaque?] :as opts}]
   (first (parse-report s (boolean opaque?) nil))))

(defn suffix-rules
  "The rules of the Public Suffix List in its text `s`, e.g. of
  public_suffix_list.dat from https://publicsuffix.org/list/, for
  public-suffix and registrable-domain. The rules are converted to ASCII,
  as a map of the :rules and the :exceptions, without their !."
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
