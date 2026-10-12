(ns dk.simongray.whatwg-url.hosts-test
  "Hosts by the examples of the URL Standard, and public suffixes by them
  and by the tests of the Public Suffix List, test_psl.txt, when
  dev/fetch.sh has put the list in dev-resources/psl."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.whatwg-url.hosts :as hosts]
            [dk.simongray.whatwg-url.test-data :as test-data]))

;; URL Standard, section 3, the table of parse-serialize roundtrips
(deftest roundtrips
  (doseq [[input domain opaque]
          [["EXAMPLE.COM" "example.com" "EXAMPLE.COM"]
           ["example%2Ecom" "example.com" "example%2Ecom"]
           ["faß.example" "xn--fa-hia.example" "fa%C3%9F.example"]
           ["0" "0.0.0.0" "0"]
           ["%30" "0.0.0.0" "%30"]
           ["0x" "0.0.0.0" "0x"]
           ["0xffffffff" "255.255.255.255" "0xffffffff"]
           ["[0:0::1]" "[::1]" "[::1]"]
           ["[0:0::1%5D" nil nil]
           ["[0:0::%31]" nil nil]
           ["09" nil "09"]
           ["example.255" nil "example.255"]
           ["example^example" nil nil]]]
    (testing (pr-str input)
      (is (= domain (hosts/parse input)))
      (is (= opaque (hosts/parse input {:opaque? true}))))))

(deftest addresses
  (is (= 2130706433 (hosts/ipv4 "0x7f.1")))
  (is (= "127.0.0.1" (hosts/serialize-ipv4 2130706433)))
  (is (= [0 0 0 0 0 0 0 1] (hosts/ipv6 "::1")))
  (is (= [0 0xffff 0 0 0 0 0x7f00 1] (hosts/ipv6 "0:ffff::127.0.0.1")))
  (is (nil? (hosts/ipv6 "1::2::3")))
  ;; URL Standard, section 3.6: the second run of zeros is longer
  (is (= "0:f::f:f:0:0" (hosts/serialize-ipv6 [0 0xf 0 0 0xf 0xf 0 0])))
  (is (hosts/ends-in-a-number? "example.1"))
  (is (hosts/ends-in-a-number? "example.0x."))
  (is (not (hosts/ends-in-a-number? "example.0xg"))))

;; URL Standard, section 3.5, the IPv4 parser and its number parser
(deftest ipv4-edges
  (doseq [[s n] [["4294967295" 4294967295]
                 ["4294967296" nil]
                 ["0x100000000" nil]
                 ["0xffffffff" 4294967295]
                 ["1.2.3.4." 16909060]
                 ["1.2.3.4.5" nil]
                 ["1.256" 16777472]
                 ["256.1" nil]
                 ["1.2.65536" nil]
                 ["1.2.65535" 16973823]
                 ["0x" 0]
                 ["0X1.0" 16777216]
                 ["010.0.0.1" 134217729]
                 ["09" nil]
                 ["08" nil]
                 ["1..2" nil]
                 ["." nil]
                 ["" nil]]]
    (is (= n (hosts/ipv4 s)) (pr-str s))))

;; URL Standard, section 3.5, the IPv6 parser, and 3.6, its serializer
(deftest ipv6-edges
  (doseq [[s pieces] [["::" [0 0 0 0 0 0 0 0]]
                      ["1:2:3:4:5:6:7:8" [1 2 3 4 5 6 7 8]]
                      ["1::" [1 0 0 0 0 0 0 0]]
                      ["::ffff:1.2.3.4" [0 0 0 0 0 0xffff 0x102 0x304]]
                      ["1:2:3:4:5:6:1.2.3.4" [1 2 3 4 5 6 0x102 0x304]]
                      ["1:2:3:4:5:6:7:8:9" nil]
                      ["1:2:3:4:5:6:7" nil]
                      ["12345::" nil]
                      [":1" nil]
                      ["1:" nil]
                      ["::1.2.3" nil]
                      ["::1.2.3.4.5" nil]
                      ["::01.2.3.4" nil]
                      ["::256.0.0.1" nil]
                      ["1:2:3:4:5:6:7:1.2.3.4" nil]
                      ["g::" nil]]]
    (is (= pieces (hosts/ipv6 s)) (pr-str s)))
  (doseq [[pieces s] [[[0 0 0 0 0 0 0 0] "::"]
                      [[1 0 0 0 0 0 0 1] "1::1"]
                      [[1 0 1 0 0 1 0 0] "1:0:1::1:0:0"]
                      [[1 0 1 1 1 1 1 1] "1:0:1:1:1:1:1:1"]
                      [[0xABCD 0 0 0 0 0 0 0] "abcd::"]]]
    (is (= s (hosts/serialize-ipv6 pieces)) (pr-str pieces))))

;; URL Standard, section 3.5, the opaque-host parser
(deftest opaque-hosts
  (is (= "%C3%BC" (hosts/parse "ü" {:opaque? true})))
  (is (= "a%20b" (hosts/parse "a%20b" {:opaque? true})))
  (is (= "[::1]" (hosts/parse "[0::1]" {:opaque? true})))
  (doseq [c [" " "#" "/" ":" "<" ">" "?" "@" "[" "\\" "]" "^" "|"]]
    (is (nil? (hosts/parse (str "a" c "b") {:opaque? true})) (pr-str c))))

(deftest domain-edges
  (testing "a forbidden domain code point, also when it's percent-encoded"
    (doseq [s ["a b" "a%20b" "a%25b" "a<b" "a%00b" "a\u007Fb"]]
      (is (nil? (hosts/parse s)) (pr-str s))))
  (testing "ASCII is only lowercased, even a label of xn-- that isn't Punycode"
    (is (= "xn--a" (hosts/parse "XN--A")))
    (is (= "xn--" (hosts/parse "xn--"))))
  (testing "a domain in Unicode that ends in a number is an IPv4 address"
    (is (= "1.2.3.4" (hosts/parse "１.２.３.４"))))
  (testing "the strict domain parser"
    (is (nil? (hosts/domain->ascii "-a.example" {:strict? true})))
    (is (nil? (hosts/domain->ascii (str (apply str (repeat 64 "a")) ".example") {:strict? true})))
    (is (= "xn--bcher-kva.example" (hosts/domain->ascii "Bücher.example" {:strict? true})))))

(deftest domains
  (is (= "xn--bcher-kva.example" (hosts/domain->ascii "Bücher.example")))
  (is (= "a_b.example" (hosts/domain->ascii "a_b.example")))
  (is (nil? (hosts/domain->ascii "a_b.example" {:strict? true})))
  (is (= "bücher.example" (hosts/domain->unicode "xn--bcher-kva.example")))
  (is (= "xn--8i7caa" (hosts/domain->unicode "xn--8i7caa")))
  (is (hosts/valid-domain? "example.com"))
  (is (not (hosts/valid-domain? "example.255")))
  (is (not (hosts/valid-domain? "-example.com"))))

;; URL Standard, section 3.2, the table of public suffixes, by a list that
;; has the rules it needs
(deftest suffixes
  (let [rules (hosts/suffix-rules "// a few rules\ncom\ngithub.io\n\nإختبار\n")]
    (doseq [[host suffix domain]
            [["com" "com" nil]
             ["example.com" "com" "example.com"]
             ["www.example.com" "com" "example.com"]
             ["sub.www.example.com" "com" "example.com"]
             ["example.com." "com." "example.com."]
             ["github.io" "github.io" nil]
             ["whatwg.github.io" "github.io" "whatwg.github.io"]
             ["xn--kgbechtv" "xn--kgbechtv" nil]
             ["example.xn--kgbechtv" "xn--kgbechtv" "example.xn--kgbechtv"]
             ["sub.example.xn--kgbechtv" "xn--kgbechtv" "example.xn--kgbechtv"]
             ["[2001:db8:85a3::8a2e:370:7334]" nil nil]]]
      (testing host
        (is (= suffix (hosts/public-suffix host rules)))
        (is (= domain (hosts/registrable-domain host rules)))))))

;; https://github.com/publicsuffix/list/wiki/Format, wildcards and
;; exceptions
(deftest wildcards-and-exceptions
  (let [rules (hosts/suffix-rules "*.ck\n!www.ck\ncom\n*.kawasaki.jp\n!city.kawasaki.jp\n")]
    (doseq [[host suffix domain]
            [["ck" "ck" nil]
             ["b.ck" "b.ck" nil]
             ["a.b.ck" "b.ck" "a.b.ck"]
             ["www.ck" "ck" "www.ck"]
             ["x.www.ck" "ck" "www.ck"]
             ["c.kawasaki.jp" "c.kawasaki.jp" nil]
             ["city.kawasaki.jp" "kawasaki.jp" "city.kawasaki.jp"]
             ["unlisted" "unlisted" nil]
             ["a.unlisted" "unlisted" "a.unlisted"]
             [".com" nil nil]
             ["1.2.3.4" nil nil]]]
      (testing host
        (is (= suffix (hosts/public-suffix host rules)))
        (is (= domain (hosts/registrable-domain host rules)))))))

(deftest public-suffix-list
  (when-let [list (test-data/text "psl/public_suffix_list.dat")]
    (let [rules (hosts/suffix-rules list)]
      (doseq [[_ input expected]
              (re-seq #"(?m)^checkPublicSuffix\('([^']*)', (?:'([^']*)'|null)\);"
                      (test-data/text "psl/test_psl.txt"))]
        (testing input
          (is (= (some-> expected hosts/parse)
                 (some-> (hosts/parse input) (hosts/registrable-domain rules)))))))))
