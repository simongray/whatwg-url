(ns dk.simongray.whatwg-url.idna-test
  "UTS #46 against the conformance tests of Unicode, IdnaTestV2.txt, with
  the flags they're made for: STD3 rules, hyphens, bidi, joiners and the
  lengths of DNS. Only whether there was an error is compared, as the test
  file allows, and the toUnicode error X4_2 of an empty label is left out,
  as UTS #46 doesn't record it.

  A test with a code point that the platform's Unicode doesn't have is
  skipped, since the platform can't normalize it."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dk.simongray.whatwg-url.idna :as idna]
            [dk.simongray.whatwg-url.punycode :as punycode]
            [dk.simongray.whatwg-url.test-data :as test-data]
            [dk.simongray.whatwg-url.unicode :as unicode]))

(def strict
  "The flags of UTS #46 that IdnaTestV2.txt is made for."
  {:check-hyphens?        true
   :check-bidi?           true
   :check-joiners?        true
   :use-std3-ascii-rules? true
   :verify-dns-length?    true})

(defn unescape
  "The field `s` of the test file with its \\uXXXX and \\x{XXXX} escapes
  undone, and \"\" as the empty string."
  [s]
  (if (= "\"\"" s)
    ""
    (str/replace s #"\\u([0-9A-Fa-f]{4})|\\x\{([0-9A-Fa-f]+)\}"
                 (fn [[_ u x]]
                   (let [c #?(:clj  (Long/parseLong (or u x) 16)
                              :cljs (js/parseInt (or u x) 16))]
                     #?(:clj  (String. (Character/toChars c))
                        :cljs (js/String.fromCharCode c)))))))

(defn statuses
  "The status codes of the field `s`, or nil when it's blank."
  [s]
  (when-not (str/blank? s)
    (set (re-seq #"[A-Z][0-9_]+" s))))

(defn cases
  "The tests of IdnaTestV2.txt, with each blank field filled in as the
  header of the file says."
  []
  (for [line  (some-> (test-data/text "unicode/IdnaTestV2.txt") str/split-lines)
        :when (not (or (str/blank? line) (str/starts-with? line "#")))
        :let  [[src u u-st n n-st t t-st] (map str/trim (str/split (first (str/split line #" # " 2)) #";"))
               src  (unescape src)
               u    (if (str/blank? u) src (unescape u))
               u-st (or (statuses u-st) #{})
               n    (if (str/blank? n) u (unescape n))
               n-st (or (statuses n-st) u-st)
               t    (if (str/blank? t) n (unescape t))
               t-st (or (statuses t-st) n-st)]]
    {:source src :unicode u :unicode-status (disj u-st "X4_2")
     :ascii-n n :ascii-n-status n-st :ascii-t t :ascii-t-status t-st}))

(defn known?
  "Whether the platform has each code point of the test `c`."
  [c]
  (every? unicode/known? ((juxt :source :unicode) c)))

(deftest idna-test-v2
  (doseq [{:keys [source] :as c} (filter known? (cases))]
    (testing (pr-str source)
      (let [{:keys [domain error?]} (idna/process source strict)]
        (is (= (boolean (seq (:unicode-status c))) error?) "toUnicode error")
        (is (= (:unicode c) domain) "toUnicode"))
      (is (= (when (empty? (:ascii-n-status c)) (:ascii-n c))
             (idna/to-ascii source strict))
          "toAsciiN")
      (is (= (when (empty? (:ascii-t-status c)) (:ascii-t c))
             (idna/to-ascii source (assoc strict :transitional? true)))
          "toAsciiT"))))

;; RFC 3492, section 7.1
(deftest punycode-samples
  (doseq [[unicode ascii] [["ليهمابتكلموشعربي؟" "egbpdaj6bu4bxfgehfvwxn"]
                           ["他们为什么不说中文" "ihqwcrb4cv8a8dqg056pqjye"]
                           ["Pročprostěnemluvíčesky" "Proprostnemluvesky-uyb24dma41a"]
                           ["-> $1.00 <-" "-> $1.00 <--"]]]
    (is (= ascii (punycode/encode unicode)))
    (is (= unicode (punycode/decode ascii)))))

(deftest punycode-edges
  (testing "the case of the basic code points is kept, and digits read in either case"
    (is (= "Bcher-2pa" (punycode/encode "BÜcher")))
    (is (= "BÜcher" (punycode/decode "Bcher-2pa")))
    (is (= "BüCHER" (punycode/decode "BCHER-KVA"))))
  (testing "the empty label, and a label of basic code points alone"
    (is (= "" (punycode/encode "")))
    (is (= "" (punycode/decode "")))
    (is (= "abc-" (punycode/encode "abc")))
    (is (= "abc" (punycode/decode "abc-"))))
  (testing "text that isn't Punycode"
    (is (nil? (punycode/decode "€-abc")) "a code point before the delimiter that isn't basic")
    (is (nil? (punycode/decode "ab!c")) "a digit that isn't one")
    (is (nil? (punycode/decode "99999999999")) "a number past 31 bits")
    (is (nil? (punycode/decode "a-9")) "digits that run out")))

;; UTS #46, section 4, with each flag on its own, against the defaults of
;; the URL Standard
(deftest the-flags
  (testing "the defaults: nontransitional, joiners, bidi, and no STD3, hyphen or length rules"
    (is (= "xn--fa-hia.de" (idna/to-ascii "Faß.de")))
    (is (= "a_b.ab--c.-x-" (idna/to-ascii "a_b.ab--c.-x-")))
    (is (= "a." (idna/to-ascii "A.")))
    (is (nil? (idna/to-ascii "a‌b")) "a joiner that no rule allows")
    (is (nil? (idna/to-ascii "faß.08.١")) "a label that starts with a digit in a bidi domain")
    (is (nil? (idna/to-ascii "a⒈com")) "a code point that would map to a dot"))
  (testing ":transitional?"
    (is (= "fass.xn--4xa" (idna/to-ascii "faß.ς" {:transitional? true})))
    (is (= "ab" (idna/to-ascii "a‌b" {:transitional? true}))))
  (testing ":check-joiners? and :check-bidi?"
    (is (= "xn--ab-j1t" (idna/to-ascii "a‌b" {:check-joiners? false})))
    (is (= "xn--11b6iv14e" (idna/to-ascii "क्‌")) "a joiner after a virama")
    (is (= "xn--fa-hia.08.xn--9hb" (idna/to-ascii "faß.08.١" {:check-bidi? false}))))
  (testing ":use-std3-ascii-rules?, :check-hyphens? and :verify-dns-length?"
    (is (nil? (idna/to-ascii "a_b" {:use-std3-ascii-rules? true})))
    (is (nil? (idna/to-ascii "ab--c" {:check-hyphens? true})))
    (is (nil? (idna/to-ascii "-a" {:check-hyphens? true})))
    (is (nil? (idna/to-ascii "a..b" {:verify-dns-length? true})))
    (is (nil? (idna/to-ascii "a." {:verify-dns-length? true})))
    (is (nil? (idna/to-ascii (apply str (repeat 64 "a")) {:verify-dns-length? true})))
    (is (some? (idna/to-ascii (apply str (repeat 63 "a")) {:verify-dns-length? true}))))
  (testing ":ignore-invalid-punycode?"
    (is (nil? (idna/to-ascii "xn--99999999999")))
    (is (= "xn--99999999999" (idna/to-ascii "xn--99999999999" {:ignore-invalid-punycode? true}))))
  (testing "toUnicode, and the error that it records"
    (is (= "bücher.de" (idna/to-unicode "xn--bcher-kva.de")))
    (is (nil? (idna/to-unicode "xn--u-ccb.com")) "Punycode of text that isn't NFC")
    (is (= {:domain "ü.com" :error? true} (idna/process "xn--u-ccb.com")))))
