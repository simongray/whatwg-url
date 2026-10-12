(ns dk.simongray.whatwg-url.percent-test
  "The percent-encode sets code point by code point, by their definitions
  in section 1.3 of the URL Standard, its examples of encoding and
  decoding, and the UTF-8 decoder of the Encoding Standard."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.whatwg-url.chars :as chars]
            [dk.simongray.whatwg-url.percent :as percent]))

(defn codes
  "The code units of the text `s`, the same way on both platforms."
  [s]
  (map #(chars/code-at s %) (range (count s))))

(def ascii-members
  "The ASCII code points of each set besides the C0 controls, as the
  standard defines the sets, each by the one before it."
  (let [query     " \"#<>"
        path      (str query "?^`{}")
        userinfo  (str path "/:;=@[\\]|")
        component (str userinfo "$%&+,")]
    {percent/c0-control-set      ""
     percent/fragment-set        " \"<>`"
     percent/query-set           query
     percent/special-query-set   (str query "'")
     percent/path-set            path
     percent/userinfo-set        userinfo
     percent/component-set       component
     percent/form-urlencoded-set (str component "!'()~")}))

(deftest the-sets
  (doseq [[set members] ascii-members
          c             (range 0x80)]
    (is (= (boolean (or (< c 0x20) (== c 0x7F) (some #{c} (codes members))))
           (boolean (set c)))
        (str "U+" c)))
  (testing "every set holds DEL and the code points above ASCII"
    (doseq [set (keys ascii-members)
            c   [0x7F 0x80 0xE9 0xFFFD 0x10FFFF]]
      (is (set c)))))

(deftest the-component-set-is-encode-uri-component
  (is (= (set (codes "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.!~*'()"))
         (set (remove percent/component-set (range 0x20 0x7F)))))
  #?(:cljs
     (doseq [c (range 0x20 0x7F)]
       (is (= (js/encodeURIComponent (js/String.fromCharCode c))
              (percent/encode (js/String.fromCharCode c)))))))

;; URL Standard, section 1.3, the summary of examples, in UTF-8
(deftest the-examples-of-the-standard
  (is (= "%E2%89%A1" (percent/encode "≡" percent/userinfo-set)))
  (is (= "%E2%80%BD" (percent/encode "‽" percent/userinfo-set)))
  (is (= "Say%20what%E2%80%BD" (percent/encode "Say what‽" percent/userinfo-set)))
  (is (= "1%2B1+%E2%89%A1+2%2520%E2%80%BD"
         (percent/encode "1+1 ≡ 2%20‽" percent/form-urlencoded-set)))
  (is (= "%%s%1G" (percent/decode "%25%s%1G")))
  (is (= [0xE2 0x80 0xBD 0x25 0x2E] (percent/decode-bytes "‽%25%2E"))))

(deftest encoding
  (testing "a set of your own is a predicate"
    (is (= "a%3Bb%20c" (percent/encode "a;b c" (some-fn percent/path-set #{0x3B})))))
  (testing "a surrogate without its pair is U+FFFD"
    (is (= "%EF%BF%BD" (percent/encode (str (char 0xD800))))))
  (testing "a code point outside the BMP is its four bytes"
    (is (= "%F0%9F%98%80" (percent/encode "😀"))))
  (is (= "" (percent/encode "")))
  (is (= "a-z_0.9~" (percent/encode "a-z_0.9~"))))

(deftest decoding
  (testing "an escape in either case, and a % that starts none"
    (is (= "a/b" (percent/decode "a%2fb")))
    (is (= "a/b" (percent/decode "a%2Fb")))
    (is (= "100%" (percent/decode "100%")))
    (is (= "%2" (percent/decode "%2")))
    (is (= "%zz" (percent/decode "%zz"))))
  (testing "a + stays a +"
    (is (= "a+b" (percent/decode "a+b"))))
  (testing "bytes that aren't UTF-8 are U+FFFD"
    (is (= "�" (percent/decode "%FF")))
    (is (= "��" (percent/decode "%FE%FF")))
    (is (= "�A" (percent/decode "%C2A"))))
  (testing "text without escapes comes back as it is"
    (is (= "ü 😀" (percent/decode "ü 😀")))))

;; Encoding Standard, 9.1.1, the UTF-8 decoder
(deftest the-utf-8-decoder
  (doseq [[bytes text] [[[0x41] "A"]
                        [[0xC3 0xA9] "é"]
                        [[0xF0 0x9F 0x98 0x80] "😀"]
                        [[0xEF 0xBB 0xBF 0x41] "﻿A"]
                        [[0xC0 0x80] "��"]
                        [[0xE0 0x80 0x80] "���"]
                        [[0xED 0xA0 0x80] "���"]
                        [[0xF4 0x90 0x80 0x80] "����"]
                        [[0xF0 0x9F 0x98] "�"]
                        [[0xF0 0x9F 0x41] "�A"]
                        [[0x80] "�"]
                        [[0xFF] "�"]
                        [[] ""]]]
    (is (= text (chars/utf-8-decode bytes)) (pr-str bytes))))

;; URL Standard, section 4.3, "URL code points"
(deftest url-code-points
  (is (every? chars/url-code-point? (codes "azAZ09!$&'()*+,-./:;=?@_~")))
  (is (not-any? chars/url-code-point? (codes " \"#%<>[\\]^`{|}")))
  (is (every? chars/url-code-point? [0xA0 0xE9 0xFFFD 0x10000 0x10FFFD]))
  (is (not-any? chars/url-code-point? [0x00 0x1F 0x7F 0x80 0x9F 0xD800 0xDFFF 0xFDD0 0xFDEF
                                       0xFFFE 0xFFFF 0x1FFFE 0x10FFFE 0x10FFFF])))
