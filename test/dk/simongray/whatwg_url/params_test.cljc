(ns dk.simongray.whatwg-url.params-test
  "Name-value pairs by the URLSearchParams tests of web-platform-tests,
  ported from urlencoded-parser.any.js and urlsearchparams-*.any.js."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.whatwg-url :as url]
            [dk.simongray.whatwg-url.params :as params]))

(deftest parsing
  (doseq [[input output]
          [["test" [["test" ""]]]
           ["﻿test=﻿" [["﻿test" "﻿"]]]
           ["%EF%BB%BFtest=%EF%BB%BF" [["﻿test" "﻿"]]]
           ["%EF%BF%BF=%EF%BF%BF" [["￿" "￿"]]]
           ["%FE%FF" [["��" ""]]]
           ["%FF%FE" [["��" ""]]]
           ["†&†=x" [["†" ""] ["†" "x"]]]
           ["%C2" [["�" ""]]]
           ["%C2x" [["�x" ""]]]
           ["_charset_=windows-1252&test=%C2x" [["_charset_" "windows-1252"] ["test" "�x"]]]
           ["" []]
           ["a" [["a" ""]]]
           ["a=b" [["a" "b"]]]
           ["a=" [["a" ""]]]
           ["=b" [["" "b"]]]
           ["&" []]
           ["&a" [["a" ""]]]
           ["a&" [["a" ""]]]
           ["a&a" [["a" ""] ["a" ""]]]
           ["a&b&c" [["a" ""] ["b" ""] ["c" ""]]]
           ["a=b&c=d" [["a" "b"] ["c" "d"]]]
           ["a=b&c=d&" [["a" "b"] ["c" "d"]]]
           ["&&&a=b&&&&c=d&" [["a" "b"] ["c" "d"]]]
           ["a=a&a=b&a=c" [["a" "a"] ["a" "b"] ["a" "c"]]]
           ["a==a" [["a" "=a"]]]
           ["a=a+b+c+d" [["a" "a b c d"]]]
           ["%=a" [["%" "a"]]]
           ["%a=a" [["%a" "a"]]]
           ["%a_=a" [["%a_" "a"]]]
           ["%61=a" [["a" "a"]]]
           ["%61+%4d%4D=" [["a MM" ""]]]
           ["id=0&value=%" [["id" "0"] ["value" "%"]]]
           ["b=%2sf%2a" [["b" "%2sf*"]]]
           ["b=%2%2af%2a" [["b" "%2*f*"]]]
           ["b=%%2a" [["b" "%*"]]]]]
    (is (= output (params/parse input)) (pr-str input))))

(deftest sorting
  (doseq [[input output]
          [["z=b&a=b&z=a&a=a" [["a" "b"] ["a" "a"] ["z" "b"] ["z" "a"]]]
           ["�=x&￼&�=a" [["￼" ""] ["�" "x"] ["�" "a"]]]
           ["ﬃ&🌈" [["🌈" ""] ["ﬃ" ""]]]
           ["é&e�&é" [["é" ""] ["e�" ""] ["é" ""]]]
           ["z=z&a=a&z=y&a=b&z=x&a=c&z=w&a=d&z=v&a=e&z=u&a=f&z=t&a=g"
            [["a" "a"] ["a" "b"] ["a" "c"] ["a" "d"] ["a" "e"] ["a" "f"] ["a" "g"]
             ["z" "z"] ["z" "y"] ["z" "x"] ["z" "w"] ["z" "v"] ["z" "u"] ["z" "t"]]]
           ["bbb&bb&aaa&aa=x&aa=y" [["aa" "x"] ["aa" "y"] ["aaa" ""] ["bb" ""] ["bbb" ""]]]
           ["z=z&=f&=t&=x" [["" "f"] ["" "t"] ["" "x"] ["z" "z"]]]
           ["a🌈&a💩" [["a🌈" ""] ["a💩" ""]]]]]
    (is (= output (params/sort (params/parse input))) (pr-str input))
    (testing "in the query of a URL"
      (let [u (url/parse (str "?" input) "https://example/")]
        (is (= output (url/search-params (url/with u :search-params (params/sort (url/search-params u)))))))))
  (is (= "http://example.com/"
         (url/href (let [u (url/parse "http://example.com/?")]
                     (url/with u :search-params (params/sort (url/search-params u))))))))

(deftest serializing
  (doseq [[pairs output]
          [[[["a" "b c"]] "a=b+c"]
           [[["a b" "c"]] "a+b=c"]
           [[["a" ""] ["a" ""] ["" "b"] ["" ""] ["" ""]] "a=&a=&=b&=&="]
           [[["" ""] ["" ""]] "=&="]
           [[["a" "b+c"] ["a+b" "c"]] "a=b%2Bc&a%2Bb=c"]
           [[["=" "a"] ["b" "="]] "%3D=a&b=%3D"]
           [[["&" "a"] ["b" "&"]] "%26=a&b=%26"]
           [[["a" "*-._"] ["*-._" "c"]] "a=*-._&*-._=c"]
           [[["a" "b%c"] ["a%b" "c"]] "a=b%25c&a%25b=c"]
           [[["a" "b\u0000c"]] "a=b%00c"]
           [[["a" "b💩c"]] "a=b%F0%9F%92%A9c"]
           [{:q "clojure podcasts" :page 2 :debug nil} "q=clojure+podcasts&page=2"]]]
    (is (= output (params/serialize pairs)) (pr-str pairs)))
  (is (= "id=0&value=%25" (params/serialize (params/parse "id=0&value=%"))))
  (is (= "a=b&c=d&e=" (params/serialize (params/parse "a=b&c=d&&e&&")))))

(deftest operations
  (let [ps (params/parse "a=1&b=2&a=3")]
    (is (= "1" (params/get ps "a")))
    (is (= "1" (params/get ps :a)))
    (is (nil? (params/get ps "c")))
    (is (= ["1" "3"] (params/get-all ps "a")))
    (is (params/has? ps "b"))
    (is (params/has? ps "a" "3"))
    (is (not (params/has? ps "a" "2")))
    (is (= [["a" "1"] ["b" "2"] ["a" "3"] ["c" "4"]] (params/append ps "c" "4")))
    (is (= [["b" "2"]] (params/delete ps "a")))
    (is (= [["a" "1"] ["b" "2"]] (params/delete ps "a" "3")))
    (is (= [["a" "x"] ["b" "2"]] (params/set ps "a" "x")))
    (is (= [["a" "1"] ["b" "2"] ["a" "3"] ["c" "x"]] (params/set ps "c" "x")))))
