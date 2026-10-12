(ns dk.simongray.whatwg-url.generative-test
  "Properties that hold for any input, tried on random URLs made of the
  parts that trip parsers up: odd schemes and separators, IPv4 numbers in
  any base, IPv6 addresses, hosts in Unicode, Punycode or percent-encoded,
  ports out of range, dot segments, backslashes, drive letters, control
  characters, lone surrogates and plain junk.

  The URL Standard's goals promise that parsing and serializing is
  idempotent, and that a URL changed by the setters doesn't change when
  it's serialized and parsed again. The rest are round trips and the
  agreement of the fast paths with the parser."
  (:require [clojure.string :as str]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [dk.simongray.whatwg-url :as url]
            [dk.simongray.whatwg-url.hosts :as hosts]
            [dk.simongray.whatwg-url.impl.chars :as chars]
            [dk.simongray.whatwg-url.idna :as idna]
            [dk.simongray.whatwg-url.params :as params]
            [dk.simongray.whatwg-url.percent :as percent]
            [dk.simongray.whatwg-url.punycode :as punycode]))

(defn- radix
  "The number `n` written in the base `r`."
  [n r]
  #?(:clj (Long/toString (long n) (int r)) :cljs (.toString n r)))

(defn- codes
  "The code units of the text `s`, the same way on both platforms."
  [s]
  (map #(chars/code-at s %) (range (count s))))

(def code-point
  "A code point of the kinds that URLs hold, the plain ones most often."
  (gen/frequency
   [[8 (gen/elements (codes "abcxyzABCXYZ0189-._~"))]
    [6 (gen/elements (codes "/\\:@?#%.!$&'()*+,;=[]{}|^`\"<> "))]
    [2 (gen/elements [0x00 0x09 0x0A 0x0D 0x1F 0x7F])]
    [3 (gen/elements [0xE9 0xDF 0xFB03 0x2460 0x1F600 0x200C 0x200D 0xFFFD 0xFEFF
                      0x5D0 0x661 0x3002 0xFF0E 0xFDD0 0x10FFFF])]
    [1 (gen/return 0xD800)]]))

(def junk
  "Text of random code points."
  (gen/fmap chars/from-code-points (gen/vector code-point 0 12)))

(def scheme
  "A scheme, special or not, in any case, or none, those of the web most
  often."
  (gen/frequency
   [[6 (gen/elements ["http" "https"])]
    [3 (gen/elements ["HTTPS" "ws" "wss" "ftp" "file" "FILE"])]
    [3 (gen/elements ["mailto" "sc" "web+demo" "blob" "javascript" "data" "urn" "a1" ""])]]))

(def separator
  "What comes between the scheme and the host, mostly ://."
  (gen/frequency
   [[6 (gen/return "://")]
    [3 (gen/elements [":" ":/" ":\\\\" ":///" "://///" "://\\" "//" ""])]]))

(def label
  "A label of a domain, of the kinds that IDNA and the IPv4 parser treat
  differently."
  (gen/elements ["example" "EXAMPLE" "a" "xn--fa-hia" "xn--" "xn--a" "faß" "bücher"
                 "ÖBB" "例え" "☕" "0x7f" "0X" "1" "08" "0" "255" "4294967295"
                 "4294967296" "-a" "a-" "ab--c" "a_b" "%41" "%2e" "%" "ﬃ" "①"
                 "a\u200Cb" "עברית" "١" "" "localhost" "a b" "^" "a%00b"]))

(def ipv4-like
  "Text that the IPv4 parser reads, in decimal, octal and hexadecimal."
  (gen/fmap #(str/join "." %)
            (gen/vector (gen/one-of [(gen/fmap str (gen/choose 0 300))
                                     (gen/fmap #(str "0x" (radix % 16)) (gen/choose 0 300))
                                     (gen/fmap #(str "0" (radix % 8)) (gen/choose 0 300))
                                     (gen/return "")])
                        1 5)))

(def ipv6-like
  "Text in brackets that the IPv6 parser reads, valid or not."
  (gen/one-of
   [(gen/elements ["[::1]" "[0:0::1]" "[::ffff:127.0.0.1]" "[1::2::3]" "[::01]" "[:1]"
                   "[ffff::127.0.0.4000]" "[]" "[::" "[0::1%5D" "[1:2:3:4:5:6:7:8:9]"])
    (gen/fmap #(str "[" (str/join ":" (map (fn [p] (radix p 16)) %)) "]")
              (gen/vector (gen/frequency [[3 (gen/return 0)] [2 (gen/choose 1 65535)]]) 1 9))
    (gen/fmap (fn [[a b]] (str "[" (str/join ":" (map #(radix % 16) a)) "::"
                               (str/join ":" (map #(radix % 16) b)) "]"))
              (gen/tuple (gen/vector (gen/choose 0 65535) 0 4)
                         (gen/vector (gen/choose 0 65535) 0 4)))]))

(def host
  "A host of any kind, or a host that's no host."
  (gen/one-of [(gen/fmap #(str/join "." %) (gen/vector label 1 4))
               ipv4-like
               ipv6-like
               (gen/return "")
               junk]))

(def userinfo
  "A username and password, or none."
  (gen/elements ["" "user@" "user:pass@" ":@" "@" "u@v@" "a:b:c@" "ü:ß@" "a b@"]))

(def port
  "A port, in range or not, or none."
  (gen/elements ["" ":" ":80" ":443" ":21" ":8080" ":0" ":65535" ":65536" ":00080" ":x"]))

(def path
  "A path of segments, with dot segments, drive letters and backslashes."
  (gen/fmap #(apply str (flatten %))
            (gen/vector (gen/tuple (gen/elements ["/" "/" "/" "\\"])
                                   (gen/one-of [(gen/elements ["" "." ".." "%2e" "%2E%2e" ".%2e"
                                                               "a" "b c" "ü" "c:" "C|" "~" "%"
                                                               "%zz" "%41" "a;b" "{}" "`"])
                                                junk]))
                        0 5)))

(def query
  "A query, or none."
  (gen/one-of [(gen/return "") (gen/fmap #(str "?" %) junk)]))

(def fragment
  "A fragment, or none."
  (gen/one-of [(gen/return "") (gen/fmap #(str "#" %) junk)]))

(def structured
  "A URL of random parts."
  (gen/fmap #(apply str %)
            (gen/tuple scheme separator userinfo host port path query fragment)))

(def input
  "Text for the parser: mostly URLs of random parts, sometimes with spaces
  and controls around them, sometimes junk."
  (gen/frequency [[6 structured]
                  [2 junk]
                  [1 (gen/fmap (fn [[ws u]] (str ws u ws))
                               (gen/tuple (gen/elements [" " "\t" "\n" "\u0000" "\u001F"])
                                          structured))]]))

(def relative
  "Text that a base resolves, of the kinds that links are."
  (gen/one-of [path (gen/fmap #(subs % (min 1 (count %))) path)
               (gen/fmap #(str "//" %) host) query fragment junk structured]))

(def base
  "A URL to resolve against, of each kind that the parser has a state for."
  (gen/one-of [(gen/elements ["https://example.com/a/b?q#f" "http://a/b/c/d;p?q" "file:///C:/demo"
                              "file://host/x" "file:///" "sc://h/p/q" "sc:opaque" "mailto:x@y"
                              "https://[::1]:8080/" "blob:https://a/b" "web+demo:/.//x"
                              "ws://h:81/" "sc:/a/b"])
               structured]))

(def attribute
  "An attribute that with sets."
  (gen/elements [:href :protocol :username :password :host :hostname :port :pathname
                 :search :hash]))

(def value
  "A value for a setter."
  (gen/one-of [junk structured host path query fragment
               (gen/elements ["" "http" "https" "file" "sc" "8080" "x" "/a/../b" "?a=b" "#x"
                              "//x" "/.//y" "c:" "a:b"])]))

(def scalar-text
  "Text of code points that aren't surrogates."
  (gen/fmap chars/from-code-points
            (gen/vector (gen/frequency [[4 (gen/choose 0x20 0x7E)]
                                        [1 (gen/choose 0 0x1F)]
                                        [2 (gen/choose 0xA0 0xD7FF)]
                                        [1 (gen/choose 0xE000 0x10FFFF)]])
                        0 16)))

;; URL Standard, "Goals": parse-then-serialize doesn't change with more of it
(defspec parsing-and-serializing-is-idempotent 4000
  (prop/for-all [s input]
    (if-let [u (url/parse s)]
      (let [h (url/href u)]
        (and (= u (url/parse h))
             (= h (url/href (url/parse h)))
             (chars/ascii? h)))
      true)))

(defspec resolving-gives-an-absolute-normal-url 3000
  (prop/for-all [b base
                 s relative]
    (let [r (url/resolve b s)]
      (or (nil? r)
          (and (= r (url/resolve nil r))
               (= r (url/resolve "https://other.example/" r))
               (= r ((url/resolver b) s)))))))

;; URL Standard, "Goals": a URL changed through the API doesn't change when
;; it's serialized and parsed again
(defspec setters-keep-urls-idempotent 3000
  (prop/for-all [b base
                 k attribute
                 v value]
    (if-let [u (url/parse b)]
      (let [changed (url/with u k v)]
        (= changed (url/parse (url/href changed))))
      true)))

(defspec the-fast-path-agrees-with-the-parser 3000
  (prop/for-all [s input]
    (let [expected (some-> (url/parse s) url/href)]
      (and (= expected (url/resolve nil s))
           (= expected (url/href s))))))

(defspec validity-is-parsing-without-errors 2000
  (prop/for-all [s input]
    (= (url/valid? s)
       (and (some? (url/parse s)) (empty? (url/validation-errors s))))))

(defspec hosts-parse-to-themselves 2000
  (prop/for-all [h host
                 opaque? gen/boolean]
    (if-let [p (hosts/parse h {:opaque? opaque?})]
      (= p (hosts/parse p {:opaque? opaque?}))
      true)))

(defspec ipv4-addresses-round-trip 500
  (prop/for-all [n (gen/large-integer* {:min 0 :max 4294967295})]
    (= n (hosts/ipv4 (hosts/serialize-ipv4 n)))))

(defspec ipv6-addresses-round-trip 500
  (prop/for-all [pieces (gen/vector (gen/frequency [[3 (gen/return 0)] [2 (gen/choose 1 65535)]]) 8)]
    (let [s (hosts/serialize-ipv6 pieces)]
      (and (= pieces (hosts/ipv6 s))
           (= (str "[" s "]") (hosts/parse (str "[" s "]")))))))

;; UTS #46, section 4: "The processing is idempotent"
(defspec idna-is-idempotent 2000
  (prop/for-all [d (gen/one-of [(gen/fmap #(str/join "." %) (gen/vector label 1 4)) junk])]
    (if-let [a (idna/to-ascii d)]
      (and (= a (idna/to-ascii a))
           (chars/ascii? a)
           (= a (some-> (idna/to-unicode a) idna/to-ascii)))
      true)))

(defspec punycode-round-trips 500
  (prop/for-all [s scalar-text]
    (if-let [e (punycode/encode s)]
      (and (chars/ascii? e) (= s (punycode/decode e)))
      true)))

(defspec percent-encoding-round-trips 500
  (prop/for-all [s scalar-text]
    (let [e (percent/encode s)]
      (and (chars/ascii? e) (= s (percent/decode e))))))

(defspec form-pairs-round-trip 500
  (prop/for-all [pairs (gen/vector (gen/tuple scalar-text scalar-text) 0 5)]
    (= pairs (params/parse (params/serialize pairs)))))

;; Encoding Standard, the UTF-8 decoder: what it gives is UTF-8 again
(defspec utf-8-decoding-gives-scalar-values 500
  (prop/for-all [bytes (gen/vector (gen/choose 0 255) 0 16)]
    (let [s (chars/utf-8-decode bytes)]
      (= s (chars/utf-8-decode (mapcat chars/utf-8 (chars/code-points s)))))))
