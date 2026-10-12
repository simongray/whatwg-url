(ns dk.simongray.whatwg-url-test
  "URLs as maps, their validation errors and validity by the examples of
  the URL Standard, and the functions around the parser."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.whatwg-url :as url]
            [dk.simongray.whatwg-url.params :as params]))

;; URL Standard, section 1.1, the examples of each error
(def error-examples
  "Inputs, and their bases, that give each validation error."
  {:domain-to-ascii                          [["https://exa%23mple.org"]]
   :domain-percent-encoded                   [["https://exam%70le.org"]]
   :host-invalid-code-point                  [["foo://exa[mple.org"]]
   :ipv4-empty-part                          [["https://127.0.0.1./"]]
   :ipv4-too-few-parts                       [["https://1.2.3/"]]
   :ipv4-too-many-parts                      [["https://1.2.3.4.5/"]]
   :ipv4-non-numeric-part                    [["https://test.42"]]
   :ipv4-non-decimal-part                    [["https://127.0.0x0.1"]]
   :ipv4-out-of-range-part                   [["https://255.255.4000.1"]]
   :ipv4-non-ascii-input                     [["https://①.②.③.④"]]
   :ipv6-unclosed                            [["https://[::1"]]
   :ipv6-invalid-compression                 [["https://[:1]"]]
   :ipv6-too-many-pieces                     [["https://[1:2:3:4:5:6:7:8:9]"]]
   :ipv6-multiple-compression                [["https://[1::1::1]"]]
   :ipv6-invalid-code-point                  [["https://[1:2:3!:4]"] ["https://[1:2:3:]"]]
   :ipv6-too-few-pieces                      [["https://[1:2:3]"]]
   :ipv6-piece-leading-zero                  [["https://[::01]"]]
   :ipv4-in-ipv6-too-many-pieces             [["https://[1:1:1:1:1:1:1:127.0.0.1]"]]
   :ipv4-in-ipv6-invalid-code-point          [["https://[ffff::.0.0.1]"] ["https://[ffff::127.0.xyz.1]"]
                                              ["https://[ffff::127.0xyz]"] ["https://[ffff::127.00.0.1]"]
                                              ["https://[ffff::127.0.0.1.2]"]]
   :ipv4-in-ipv6-out-of-range-part           [["https://[ffff::127.0.0.4000]"]]
   :ipv4-in-ipv6-too-few-parts               [["https://[ffff::127.0.0]"]]
   :invalid-url-unit                         [["https://example.org/>"] [" https://example.org "]
                                              ["ht\ntps://example.org"] ["https://example.org/%s"]]
   :special-scheme-missing-following-solidus [["file:c:/my-secret-folder"] ["https:example.org"]
                                              ["https:foo.html" "https://example.org/"]]
   :missing-scheme-non-relative-url          [["💩"] ["💩" "mailto:user@example.org"]]
   :invalid-reverse-solidus                  [["https://example.org\\path\\to\\file"]]
   :invalid-credentials                      [["https://user@example.org"] ["ssh://user@example.org"]]
   :host-missing                             [["https://#fragment"] ["https://:443"] ["https://user:pass@"]]
   :port-out-of-range                        [["https://example.org:70000"]]
   :port-invalid                             [["https://example.org:7z"]]
   ;; the standard's example is /c:/path/to/file, which the file slash
   ;; state reads without the error
   :file-invalid-windows-drive-letter        [["c|/path/to/file" "file:///c:/"]]
   :file-invalid-windows-drive-letter-host   [["file://c:"]]})

(deftest validation-errors
  (doseq [[error examples] error-examples
          [input base]     examples]
    (testing (pr-str input base)
      (is (some #{error} (url/validation-errors input base)))
      (is (not (url/valid? input base))))))

;; URL Standard, section 4, the table of how inputs parse
(deftest valid-urls
  (doseq [[input base valid? output]
          [["https:example.org" nil false "https://example.org/"]
           ["https://////example.com///" nil false "https://example.com///"]
           ["https://example.com/././foo" nil true "https://example.com/foo"]
           ["hello:world" "https://example.com/" true "hello:world"]
           ["https:example.org" "https://example.com/" false "https://example.com/example.org"]
           ["\\example\\..\\demo/.\\" "https://example.com/" false "https://example.com/demo/"]
           ["example" "https://example.com/demo" true "https://example.com/example"]
           ["file:///C|/demo" nil false "file:///C:/demo"]
           [".." "file:///C:/demo" true "file:///C:/"]
           ["file://localhost/" nil true "file:///"]
           ["file://loc%61lhost/" nil false "file:///"]
           ["https://user:password@example.org/" nil false "https://user:password@example.org/"]
           ["https://example.org/foo bar" nil false "https://example.org/foo%20bar"]
           ["https://EXAMPLE.com/../x" nil true "https://example.com/x"]
           ["https://ex ample.org/" nil false nil]
           ["example" nil false nil]
           ["https://example.com:demo" nil false nil]
           ["http://[www.example.com]/" nil false nil]
           ["https://example.org//" nil true "https://example.org//"]
           ["https://example.com/[]?[]#[]" nil false "https://example.com/[]?[]#[]"]
           ["https://example/%?%#%" nil false "https://example/%?%#%"]
           ["https://example/%25?%25#%25" nil true "https://example/%25?%25#%25"]]]
    (testing (pr-str input base)
      (is (= valid? (url/valid? input base)))
      (is (= output (url/resolve base input))))))

;; Found by comparing random URLs with Node 24 (ada 3.2.3), which differs
;; from the standard on these, while WPT has none of them
(deftest where-wpt-is-silent
  (testing "a relative URL against an opaque path fails, whatever it holds"
    ;; the no scheme state: base has an opaque path and c is not #
    (is (nil? (url/resolve "mailto:x@y" "X8#@")))
    (is (nil? (url/resolve "mailto:x@y" "x")))
    (is (= "mailto:x@y#f" (url/resolve "mailto:x@y" "#f"))))
  (testing "an empty query of the base is kept"
    ;; the relative state copies the query, which is the empty string
    (is (= "http://a/b?#x" (url/resolve "http://a/b?" "#x")))
    (is (= "http://a/b?" (url/resolve "http://a/b?#y" ""))))
  (testing "a double dot at the end leaves the empty path segment"
    ;; the path state: "for input /usr/.. the result is / and not a lack of
    ;; a path"
    (is (= "sc://h/" (url/href "sc://h/..")))
    (is (= "sc://h/" (url/resolve "sc://h/p/q" "/..")))
    (is (= "sc://h/" (url/resolve "sc://h/p/q" "/%2E%2e")))
    (is (= "web+demo:/" (url/resolve "web+demo:/.//x" ".%2e/.."))))
  (testing "only a drive letter alone is kept at the start of a file path"
    ;; shorten a url's path: a normalized Windows drive letter is two code
    ;; points
    (is (= "file:///c" (url/href "file:///a:b/../c")))
    (is (= "file:///a:/c" (url/href "file:///a:/../c")))
    (is (= "file:///a:/c" (url/href "file:///a|/../c"))))
  (testing "a domain with a right-to-left label meets the bidi rule in every label"
    ;; domain parser ToASCII: CheckBidi is true, so neither 08 nor an
    ;; Arabic-Indic digit can start a label
    (is (nil? (url/parse "http://faß.08.١/")))
    (is (nil? (url/parse "http://faß.١/")))
    (is (= "http://xn--fa-hia.xn--5dbqzzl/" (url/href "http://faß.עברית/")))))

(deftest urls-as-maps
  (testing "only the parts a URL has"
    (is (= {:scheme "https" :host "example.com" :path [""]}
           (url/parse "HTTPS://EXAMPLE.COM:443")))
    (is (= {:scheme "https" :username "ann" :host "example.com" :port 8443
            :path ["a" "c%20d"] :query "x%20y" :fragment ""}
           (url/parse "https://ann:@example.com:8443/a/./b/../c d?x y#")))
    (is (= {:scheme "mailto" :path "me@example.com" :query "subject=hi"}
           (url/parse "mailto:me@example.com?subject=hi")))
    (is (= {:scheme "file" :host "" :path ["etc" "hosts"]}
           (url/parse "file:///etc/hosts"))))
  (testing "a map that's written as a URL"
    (is (= "https://example.com/feed.xml?page=2"
           (url/href {:scheme "https" :host "example.com" :path ["feed.xml"] :query "page=2"})))
    (is (= "https://example.com/#top"
           (url/href "https://example.com/#top")))
    (is (= "https://example.com/"
           (url/href "https://example.com/#top" {:fragment? false})))))

(deftest resolving
  (let [page "https://example.com/blog/post?x=1#top"]
    (is (= "https://example.com/about" (url/resolve page "../about")))
    (is (= "https://example.com/blog/post?y=2" (url/resolve page "?y=2")))
    (is (= "https://example.com/blog/post?x=1#bottom" (url/resolve page "#bottom")))
    (is (= "https://example.com/blog/post?x=1" (url/resolve page "")))
    (is (= "https://cdn.example/x.js" (url/resolve page "//cdn.example/x.js")))
    (is (= "https://example.com/about" (url/resolve (url/parse page) "/about")))
    (is (nil? (url/resolve nil "/about")))
    (is (nil? (url/resolve "not a base" "/about")))
    (is (nil? (url/resolve page nil)))
    (testing "many links against a base parsed once"
      (is (= ["https://example.com/blog/a" "https://example.com/b" "https://x.example/" nil]
             (map (url/resolver page) ["a" "/b" "https://x.example" nil])))
      (is (= [nil nil] (map (url/resolver "not a base") ["a" "https://x.example/"]))))))

(deftest text-of-a-url
  #?(:clj
     (testing "anything that prints as a URL"
       (is (= "example.com" (url/hostname (java.net.URI. "https://Example.com/a"))))
       (is (= "https" (url/scheme (java.net.URL. "https://example.com/"))))
       (is (= ["b" "a"] (:path (url/parse "a" (java.net.URI. "https://example.com/b/c")))))))
  (testing "in the order a URL is written"
    (is (= [:scheme :username :host :port :path :query :fragment]
           (keys (url/parse "https://u@example.com:8443/a?q#f")))))
  (is (nil? (url/scheme nil)))
  (is (nil? (url/scheme "relative/path"))))

(deftest attributes
  (let [u "https://ann:secret@example.com:8443/a/b?q=1#top"]
    (is (= ["https://ann:secret@example.com:8443/a/b?q=1#top" "https://example.com:8443" "https:"
            "ann" "secret" "example.com:8443" "example.com" "8443" "/a/b" "?q=1" "#top"]
           ((juxt url/href url/origin url/protocol url/username url/password url/host
                  url/hostname url/port url/pathname url/search url/hash)
            u)))
    (is (= [["q" "1"]] (url/search-params u)))
    (is (nil? (url/hostname "not a URL"))))
  (testing "of URLs without a host, a port, a query or a fragment"
    (doseq [[u attrs] [["mailto:me@example.com" ["mailto:" "" "" "me@example.com" "" "" "null" []]]
                       ["file:///etc/hosts" ["file:" "" "" "/etc/hosts" "" "" "null" []]]
                       ["sc://host" ["sc:" "host" "host" "" "" "" "null" []]]
                       ["https://example.com/?#" ["https:" "example.com" "example.com" "/" "" "" "https://example.com" []]]]]
      (is (= attrs ((juxt url/protocol url/host url/hostname url/pathname url/search url/hash
                          url/origin url/search-params)
                    u))
          u)))
  (testing "the default port of each special scheme is left out"
    (is (= ["http://a" "https://a" "ws://a" "wss://a" "ftp://a" "http://a:443"]
           (map url/origin ["http://a:80" "https://a:443" "ws://a:80" "wss://a:443" "ftp://a:21"
                            "http://a:443"])))))

;; URL Standard, section 6.1, each setter where it refuses a value or changes
;; more than its part
(deftest each-setter
  (testing "protocol: between special schemes, or between others, and not to file with credentials"
    (is (= "https://a/" (url/href (url/with "http://a:443/" :protocol "https"))))
    (is (= "ws://a/" (url/href (url/with "https://a/" :protocol "ws:"))))
    (is (= "https://a/" (url/href (url/with "https://a/" :protocol "sc"))))
    (is (= "sc://a/" (url/href (url/with "sc://a/" :protocol "https"))))
    (is (= "foo://a/" (url/href (url/with "sc://a/" :protocol "foo"))))
    (is (= "http://u@a/" (url/href (url/with "http://u@a/" :protocol "file"))))
    (is (= "file:///x" (url/href (url/with "file:///x" :protocol "http"))))
    (is (= "https://a/" (url/href (url/with "https://a/" :protocol "1http")))))
  (testing "username and password: percent-encoded, and not for a URL without a host"
    (is (= "https://a%20b:c%40d@x/" (url/href (url/with "https://x/" :username "a b" :password "c@d"))))
    (is (= "https://x/" (url/href (url/with "https://u:p@x/" :username "" :password ""))))
    (is (= "file:///x" (url/href (url/with "file:///x" :username "u"))))
    (is (= "sc:/x" (url/href (url/with "sc:/x" :password "p")))))
  (testing "host and hostname: the port with host alone, and no empty host for a special URL"
    (is (= "https://b:8080/" (url/href (url/with "https://a/" :host "b:8080"))))
    (is (= "https://b:1/" (url/href (url/with "https://a:1/" :host "b"))))
    (is (= "https://a/" (url/href (url/with "https://a/" :hostname "b:8080"))))
    (is (= "https://a/" (url/href (url/with "https://a/" :host ""))))
    (is (= "sc:///" (url/href (url/with "sc://a/" :host ""))))
    (is (= "sc://u@a/" (url/href (url/with "sc://u@a/" :host ""))))
    (is (= "file://b/x" (url/href (url/with "file://a/x" :host "b"))))
    (is (= "file:///x" (url/href (url/with "file://a/x" :host "localhost")))))
  (testing "port: the digits before anything else, and none out of range"
    (is (= "https://a:8080/" (url/href (url/with "https://a/" :port "8080abc"))))
    (is (= "https://a/" (url/href (url/with "https://a:1/" :port "443"))))
    (is (= "https://a:1/" (url/href (url/with "https://a:1/" :port "99999"))))
    (is (= "https://a:1/" (url/href (url/with "https://a:1/" :port "x"))))
    (is (= "https://a/" (url/href (url/with "https://a:1/" :port ""))))
    (is (= "https://a:8080/" (url/href (url/with "https://a/" :port 8080))))
    (is (= "file:///x" (url/href (url/with "file:///x" :port "1")))))
  (testing "pathname: ? and # are encoded, and a special path starts with /"
    (is (= "https://a/a%20b%3Fc%23d" (url/href (url/with "https://a/" :pathname "a b?c#d"))))
    (is (= "https://a/b" (url/href (url/with "https://a/x/y" :pathname "/a/../b"))))
    (is (= "sc://h/x" (url/href (url/with "sc://h" :pathname "x"))))
    (is (= "sc:/.//x" (url/href (url/with "sc:/a" :pathname "//x")))))
  (testing "search and hash: without their ? and #, and nothing at all when empty"
    (is (= "https://a/?a%23b" (url/href (url/with "https://a/" :search "a#b"))))
    (is (= "https://a/?q" (url/href (url/with "https://a/" :search "?q"))))
    (is (= "https://a/??q" (url/href (url/with "https://a/" :search "??q"))))
    (is (= "https://a/?" (url/href (url/with "https://a/" :search "?"))))
    (is (= "https://a/" (url/href (url/with "https://a/?q" :search ""))))
    (is (= "https://a/#a%20b" (url/href (url/with "https://a/" :hash "#a b"))))
    (is (= "https://a/" (url/href (url/with "https://a/#x" :hash ""))))
    (is (= "mailto:x?q#f" (url/href (url/with "mailto:x" :search "q" :hash "f")))))
  (testing "href: another URL, unless it doesn't parse"
    (is (= "https://b/" (url/href (url/with "https://a/" :href "HTTPS://B"))))
    (is (= "https://a/" (url/href (url/with "https://a/" :href "/relative"))))))

(deftest setting-attributes
  (testing "in order"
    (is (= "http://example.org:8080/c%20d#top"
           (url/href (url/with "https://example.com/a?b"
                               :protocol "http" :host "example.org:8080"
                               :pathname "/c d" :search "" :hash "top")))))
  (testing "a value that the setter refuses"
    (is (= "https://example.com/" (url/href (url/with "https://example.com/" :hostname "a:b"))))
    (is (= "mailto:me@example.com" (url/href (url/with "mailto:me@example.com" :pathname "x")))))
  (testing "the pairs of the query"
    (is (= "https://example.com/?q=clojure+podcasts&page=2"
           (url/href (url/with "https://example.com/" :search-params {:q "clojure podcasts" :page 2}))))
    (is (= "https://example.com/"
           (url/href (url/with "https://example.com/?a=1" :search-params [])))))
  (testing "an attribute that the URL class doesn't have"
    (is (thrown? #?(:clj Exception :cljs js/Error) (url/with "https://example.com/" :path "/x")))))

(deftest updating-attributes
  (is (= "https://example.com/?q=a&page=2"
         (url/href (url/update "https://example.com/?q=a" :search-params params/set "page" 2))))
  (is (= "https://example.com/?q=b&page=2"
         (url/href (url/update "https://example.com/?q=a&page=2&q=c" :search-params params/set "q" "b"))))
  (is (= "https://example.com/api/feed.xml"
         (url/href (url/update "https://example.com/api/" :pathname str "feed.xml"))))
  (is (= "https://example.com:8444/"
         (url/href (url/update "https://example.com:8443/" :port #(str (inc (parse-long %)))))))
  (is (nil? (url/update "not a url" :hash str "x"))))

(deftest origins
  (is (= "https://example.com" (url/origin "https://EXAMPLE.com:443/x")))
  (is (= "https://whatwg.org" (url/origin "blob:https://whatwg.org/d0360e2f-caee-469f-9a2f-87d5b0456f6f")))
  (is (= "null" (url/origin "file:///etc/hosts")))
  (is (= "null" (url/origin "blob:file:///etc/hosts")))
  (is (url/same-origin? "https://example.com/a" "https://example.com:443/b"))
  (is (not (url/same-origin? "https://example.com/" "http://example.com/")))
  (is (not (url/same-origin? "file:///a" "file:///a"))))

(deftest equivalence
  (is (url/equivalent? "HTTPS://example.com" "https://example.com/"))
  (is (not (url/equivalent? "https://example.com/#a" "https://example.com/#b")))
  (is (url/equivalent? "https://example.com/#a" "https://example.com/#b" {:fragment? false}))
  (is (url/path-starts-with? "https://example.com/a/b" "https://example.com/a"))
  (is (not (url/path-starts-with? "https://example.com/ab" "https://example.com/a")))
  (is (not (url/path-starts-with? "web+demo:a/b" "web+demo:a"))))
