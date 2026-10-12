(ns dk.simongray.whatwg-url.punycode-test
  "Punycode by the samples of RFC 3492, and at its edges: the case of the
  basic code points, empty and basic labels, and text that isn't Punycode."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.whatwg-url.punycode :as punycode]))

;; RFC 3492, section 7.1
(deftest samples
  (doseq [[unicode ascii] [["ليهمابتكلموشعربي؟" "egbpdaj6bu4bxfgehfvwxn"]
                           ["他们为什么不说中文" "ihqwcrb4cv8a8dqg056pqjye"]
                           ["Pročprostěnemluvíčesky" "Proprostnemluvesky-uyb24dma41a"]
                           ["-> $1.00 <-" "-> $1.00 <--"]]]
    (is (= ascii (punycode/encode unicode)))
    (is (= unicode (punycode/decode ascii)))))

(deftest edges
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
