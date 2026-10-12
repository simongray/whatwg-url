(ns dk.simongray.whatwg-url.idna
  "Domain names in Unicode and in ASCII, by Unicode IDNA Compatibility
  Processing (UTS #46) of Unicode 18.0.0, revision 36.

  The options are those of UTS #46, and default-options holds the ones
  that the URL Standard uses for the domain of a URL. NFC comes from the
  platform. A domain is an error rather than a guess when it has a new
  code point that NFC doesn't leave alone and the platform doesn't know."
  (:require [clojure.string :as str]
            [dk.simongray.whatwg-url.chars :as chars]
            [dk.simongray.whatwg-url.punycode :as punycode]
            [dk.simongray.whatwg-url.unicode :as unicode]))

(def default-options
  "The options that the URL Standard uses for the domain of a URL, both in
  ToASCII when it isn't strict and in ToUnicode:

  - :check-hyphens?, true to refuse a hyphen at the start or the end of a
    label, or in its third and fourth place
  - :check-bidi?, true to check right-to-left labels by the rules of
    RFC 5893
  - :check-joiners?, true to check U+200C and U+200D by the CONTEXTJ
    rules of RFC 5892
  - :use-std3-ascii-rules?, true to allow only a-z, 0-9 and - of ASCII
  - :transitional?, true for the deprecated mapping of ß, ς, U+200C and
    U+200D
  - :verify-dns-length?, true to check the lengths that DNS allows, with
    no empty label
  - :ignore-invalid-punycode?, true to keep a label of xn-- that doesn't
    decode as it is"
  {:check-hyphens?           false
   :check-bidi?              true
   :check-joiners?           true
   :use-std3-ascii-rules?    false
   :transitional?            false
   :verify-dns-length?       false
   :ignore-invalid-punycode? false})

;; UTS #46, section 4, step 1
(defn- mapped
  "The text `s` mapped by the IDNA mapping table, with the deviations
  mapped too when `transitional?`."
  [s transitional?]
  (let [b (chars/builder)
        n (chars/length s)]
    (loop [i 0]
      (when (< i n)
        (let [c (chars/code-point-at s i)]
          ;; the table maps no ASCII but A to Z, which it makes lower case,
          ;; and two comparisons, as only those of two numbers are inlined
          (if (< c 0x80)
            (chars/append-code-point! b (if (and (<= 0x41 c) (<= c 0x5A)) (+ c 0x20) c))
            (let [[status mapping] (unicode/idna-status c)]
              (case status
                :mapped    (chars/append! b (if (and transitional? (== c 0x1E9E)) "ss" mapping))
                :deviation (if transitional?
                             (chars/append! b mapping)
                             (chars/append-code-point! b c))
                :ignored   nil
                (chars/append-code-point! b c))))
          (recur (+ i (chars/width c))))))
    (str b)))

(defn- virama-at?
  "Whether the code points `cps` have a virama at `i`."
  [cps i]
  (and (<= 0 i) (unicode/virama? (nth cps i))))

(defn- joins?
  "Whether the first code point of the code points `cps` from `i` on in the
  direction `step` (1 or -1), past those of the joining type T, has a
  joining type of `types`."
  [cps i step types]
  (loop [i i]
    (when (< -1 i (count cps))
      (let [t (unicode/joining-type (nth cps i))]
        (cond
          (= :T t)  (recur (+ i step))
          (types t) true
          :else     false)))))

;; RFC 5892, appendix A.1 and A.2
(defn- joiners-ok?
  "Whether each U+200C and U+200D of the code points `cps` of a label is
  allowed by the CONTEXTJ rules."
  [cps]
  (every? (fn [i]
            (case (long (nth cps i))
              0x200C (or (virama-at? cps (dec i))
                         (and (joins? cps (dec i) -1 #{:L :D})
                              (joins? cps (inc i) 1 #{:R :D})))
              0x200D (virama-at? cps (dec i))
              true))
          (range (count cps))))

;; RFC 5893, section 2
(defn- bidi-ok?
  "Whether the code points `cps` of a label of a bidi domain name meet the
  six conditions of the bidi rule."
  [cps]
  (let [classes (mapv unicode/bidi-class cps)
        end     (last (remove #{:NSM} classes))]
    (case (first classes)
      (:R :AL) (and (every? #{:R :AL :AN :EN :ES :CS :ET :ON :BN :NSM} classes)
                    (#{:R :AL :EN :AN} end)
                    (not (and (some #{:EN} classes) (some #{:AN} classes))))
      :L       (and (every? #{:L :EN :ES :CS :ET :ON :BN :NSM} classes)
                    (#{:L :EN} end))
      false)))

(chars/defscan joiner-at
  "The index in the text `s` from `i` of its first U+200C or U+200D, or
  its end."
  [s i] [x] (or (== 0x200C x) (== 0x200D x)))

;; the table maps no ASCII but A to Z, so ASCII needs no lookup in it
(defn- valid-code-point?
  "Whether the code point `c` may be in a label by the IDNA mapping table,
  with the deviations valid unless `transitional?`, and by the STD3 rules
  when `std3?`."
  [^long c transitional? std3?]
  (if (< c 0x80)
    (and (not (and (<= 0x41 c) (<= c 0x5A)))
         (or (not std3?)
             (== 0x2D c)
             (chars/ascii-digit? c)
             (and (<= 0x61 c) (<= c 0x7A))))
    (let [[status] (unicode/idna-status c)]
      (or (= :valid status) (and (= :deviation status) (not transitional?))))))

;; a loop over the text rather than a vector of its code points
(defn- valid-code-points?
  "Whether valid-code-point? takes each code point of the `label`, with
  `transitional?` and `std3?`."
  [label transitional? std3?]
  (let [n (chars/length label)]
    (loop [i 0]
      (or (== i n)
          (let [c (chars/code-point-at label i)]
            (and (valid-code-point? c transitional? std3?)
                 (recur (+ i (chars/width c)))))))))

;; UTS #46, section 4.1, all but the bidi rule, which takes the whole domain
(defn- valid-label?
  "Whether the non-empty `label` meets the validity criteria with `opts`,
  for a label that was decoded from Punycode when `decoded?`."
  [label decoded? {:keys [check-hyphens? check-joiners? use-std3-ascii-rules?
                          transitional?]}]
  (and (or (not decoded?)
           (and (unicode/known? label) (= label (unicode/nfc label))))
       (if check-hyphens?
         ;; the third and fourth code points, which needn't be the third
         ;; and fourth code units
         (let [cps (chars/code-points label)]
           (not (or (and (<= 4 (count cps)) (= [0x2D 0x2D] (subvec cps 2 4)))
                    (str/starts-with? label "-")
                    (str/ends-with? label "-"))))
         (not (str/starts-with? label "xn--")))
       (not (str/includes? label "."))
       (not (unicode/mark? (chars/code-point-at label 0)))
       (valid-code-points? label transitional? use-std3-ascii-rules?)
       (or (not check-joiners?)
           ;; most labels have no joiner, which one quick look at each code
           ;; unit tells
           (== (chars/length label) (joiner-at label 0))
           (joiners-ok? (chars/code-points label)))))

;; UTS #46, section 4, step 4
(defn- converted
  "The `label` converted from Punycode when it starts with xn--, and
  whether it's valid by `opts`, as a pair."
  [label opts]
  (cond
    (= "" label)
    [label true]

    (not (str/starts-with? label "xn--"))
    [label (valid-label? label false opts)]

    (not (chars/ascii? label))
    [label false]

    :else
    (if-let [decoded (punycode/decode (subs label 4))]
      [decoded (and (not= "" decoded)
                    (not (chars/ascii? decoded))
                    (valid-label? decoded true (assoc opts :transitional? false)))]
      [label (boolean (:ignore-invalid-punycode? opts))])))

(defn- bidi-domain?
  "Whether the `domain` has a code point of the bidi class R, AL or AN,
  which no ASCII code point has."
  [domain]
  (let [n (chars/length domain)]
    (loop [i 0]
      (and (< i n)
           (let [c (chars/code-point-at domain i)]
             (or (and (<= 0x80 c) (contains? #{:R :AL :AN} (unicode/bidi-class c)))
                 (recur (+ i (chars/width c)))))))))

;; UTS #46, section 4.1, the bidi rule over the whole domain
(defn- bidi-rule-ok?
  "Whether the `labels` of the `domain` meet the bidi rule, which only a
  domain with a right-to-left code point has to."
  [labels domain]
  (or (not (bidi-domain? domain))
      (every? #(or (= "" %) (bidi-ok? (chars/code-points %))) labels)))

;; UTS #46, section 4
(defn process
  "The Unicode `domain` mapped, normalized and converted by the processing
  steps of UTS #46 with `opts`, as a map of the :domain and whether an
  :error? was recorded. The `opts` override default-options."
  ([domain]
   (process domain {}))
  ([domain opts]
   (let [{:keys [check-bidi? transitional?] :as opts} (merge default-options opts)
         s       (mapped domain transitional?)
         ascii?  (chars/ascii? s)
         known?  (or ascii? (unicode/known? s))
         s       (if ascii? s (unicode/nfc s))
         results (mapv #(converted % opts) (str/split s #"\." -1))
         labels  (mapv first results)
         domain  (str/join "." labels)]
     {:domain domain
      :error? (not (and known?
                        (every? second results)
                        (or (not check-bidi?) (bidi-rule-ok? labels domain))))})))

(defn- dns-length-ok?
  "Whether the ASCII `labels` of a domain are each 1 to 63 long and are 1
  to 253 long together, with no empty root label."
  [labels]
  (and (every? #(<= 1 (count %) 63) labels)
       (<= (count (str/join "." labels)) 253)))

;; UTS #46, section 4.2
(defn to-ascii
  "The `domain` in ASCII by Unicode ToASCII with `opts`, or nil when an
  error was recorded. The `opts` override default-options.

      (to-ascii \"Bücher.de\")
      ;; => \"xn--bcher-kva.de\""
  ([domain]
   (to-ascii domain {}))
  ([domain opts]
   (let [{:keys [domain error?]} (process domain opts)
         labels                  (map #(if (chars/ascii? %)
                                         %
                                         (some->> (punycode/encode %) (str "xn--")))
                                      (str/split domain #"\." -1))]
     (when (and (not error?)
                (every? some? labels)
                (or (not (:verify-dns-length? opts)) (dns-length-ok? labels)))
       (str/join "." labels)))))

;; UTS #46, section 4.3
(defn to-unicode
  "The `domain` in Unicode by Unicode ToUnicode with `opts`, or nil when an
  error was recorded. The `opts` override default-options.

      (to-unicode \"xn--bcher-kva.de\")
      ;; => \"bücher.de\""
  ([domain]
   (to-unicode domain {}))
  ([domain opts]
   (let [{:keys [domain error?]} (process domain opts)]
     (when-not error?
       domain))))

(comment
  (to-ascii "Faß.de")
  (to-ascii "Faß.de" {:transitional? true})
  (to-ascii "日本語。ＪＰ")
  (to-ascii "a⒈com")
  (process "xn--u-ccb.com")
  #_.)
