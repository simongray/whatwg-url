(ns dk.simongray.whatwg-url.wpt-test
  "The URL library against the URL tests of web-platform-tests, from the
  files that dev/fetch.sh puts in dev-resources/wpt, and the tests that
  WPT writes in JavaScript, ported: the stripping of the setters, and
  percent-encoding in UTF-8.

  A test with a code point that the platform's Unicode doesn't have is
  skipped, since IDNA refuses a host with one."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.whatwg-url :as url]
            [dk.simongray.whatwg-url.params :as params]
            [dk.simongray.whatwg-url.test-data :as test-data]
            [dk.simongray.whatwg-url.unicode :as unicode]))

(def attributes
  "The getter of each attribute of the URL class."
  {"href"     url/href
   "origin"   url/origin
   "protocol" url/protocol
   "username" url/username
   "password" url/password
   "host"     url/host
   "hostname" url/hostname
   "port"     url/port
   "pathname" url/pathname
   "search"   url/search
   "hash"     url/hash})

(defn cases
  "The tests of the WPT file `name`, without its comments."
  [name]
  (filter map? (test-data/json (str "wpt/" name))))

(defn known?
  "Whether the platform has each code point of the texts `ss`."
  [& ss]
  (every? #(or (nil? %) (unicode/known? %)) ss))

(defn attributes-of
  "The attributes of the `url` that `expected` has, as it has them."
  [url expected]
  (into {} (for [[k f] attributes :when (contains? expected k)] [k (f url)])))

(deftest urltestdata
  (doseq [{:strs [input base failure relativeTo] :as c}
          (concat (cases "urltestdata.json") (cases "urltestdata-javascript-only.json"))
          :when (known? input base)]
    (testing (str (pr-str input) " against " (pr-str base))
      (let [u (url/parse input base)]
        (if failure
          (do (is (nil? u))
              (when relativeTo
                ;; the URL of a test page of WPT, by the input's scheme
                (is (some? (url/parse input (str (or (second (re-find #"^([a-z]+):" input)) "http")
                                                 "://web-platform.test:8000/")))
                    "against a base")
                (when (= "any-base" relativeTo)
                  (is (some? (url/parse input "sc:opaque")) "against an opaque base"))))
          (do (is (= (select-keys c (keys attributes)) (attributes-of u c)))
              (when (contains? c "searchParams")
                (is (= (c "searchParams") (params/serialize (url/search-params u)))))
              (testing "parsed again"
                (is (= u (url/parse (url/href u)))))))))))

;; resolve and href give a URL that the parser would give back as it is
;; without parsing it, which the full parse must agree with
(deftest normal-urls
  (doseq [s     (->> (concat (cases "urltestdata.json") (cases "toascii.json"))
                     (mapcat (juxt #(get % "input") #(get % "href")))
                     (concat (map #(str "http://" % "/") ["1.2" "a.0x" "a.1." "a..b" "xn--a" "0x"]))
                     (filter string?))
          :when (known? s)]
    (let [expected (some-> (url/parse s) url/href)]
      (is (= expected (url/resolve nil s)) (pr-str s))
      (is (= expected (url/href s)) (pr-str s)))))

(deftest setters
  (doseq [[attribute cases] (dissoc (test-data/json "wpt/setters_tests.json") "comment")
          {:strs [href new_value expected]} cases
          :when (known? href new_value)]
    (testing (str href " ." attribute " = " (pr-str new_value))
      (is (= expected (attributes-of (url/with href (keyword attribute) new_value) expected))))))

(deftest toascii
  (doseq [{:strs [input output]} (concat (cases "toascii.json") (cases "IdnaTestV2.json"))
          :when (and (not= "" input) (known? input))]
    (testing (pr-str input)
      (let [u (url/parse (str "https://" input "/x"))]
        (if output
          (is (= [output output "/x" (str "https://" output "/x")]
                 ((juxt url/host url/hostname url/pathname url/href) u)))
          (is (nil? u))))
      (doseq [k [:host :hostname]]
        (is (= (or output "x") ((attributes (name k)) (url/with "https://x/x" k input)))
            (name k))))))

(deftest percent-encoding
  (doseq [{:strs [input output]} (cases "percent-encoding.json")]
    (let [u (url/parse (str "https://doesnotmatter.invalid/?" input "#" input))]
      (is (= [(str "?" (output "utf-8")) (str "#" (output "utf-8"))]
             [(url/search u) (url/hash u)])))))

(defn url-string
  "The text of a URL of the setter-stripping tests, with `parts` in place
  of its defaults."
  [parts]
  (let [{:keys [scheme username password host port pathname search hash]}
        (merge {:scheme "https" :username "username" :password "password" :host "host"
                :port "8000" :pathname "path" :search "query" :hash "fragment"}
               parts)]
    (str scheme "://" username ":" password "@" host ":" port "/" pathname "?" search "#" hash)))

;; url-setters-stripping.any.js
(deftest setters-stripping
  (doseq [scheme ["https" "wpt++"]
          i      [0x00 0x09 0x0A 0x0D 0x1F]
          :let   [stripped? (#{0x09 0x0A 0x0D} i)
                  https?    (= "https" scheme)
                  cp        (str (char i))
                  encoded   (str "%" (when (< i 0x10) "0") #?(:clj (format "%X" i) :cljs (.toUpperCase (.toString i 16))))
                  u         (url/parse (url-string {:scheme scheme}))]]
    (testing (str scheme " " i)
      (let [expected (if https? (if stripped? "http" "https") (if stripped? "wpt--" "wpt++"))]
        (doseq [input [(str cp (if https? "http" "wpt--"))
                       (str (if https? "http" "wpt--") cp)]]
          (is (= (url-string {:scheme expected}) (url/href (url/with u :protocol input))) "protocol")))
      (doseq [k                      [:username :password]
              [expected-part input] [[(str encoded "test") (str cp "test")]
                                     [(str "te" encoded "st") (str "te" cp "st")]
                                     [(str "test" encoded) (str "test" cp)]]]
        (is (= (url-string {:scheme scheme k expected-part}) (url/href (url/with u k input)))
            (name k)))
      (doseq [k                     [:host :hostname]
              [expected-part input] [[(str (if https? cp encoded) "test") (str cp "test")]
                                     [(str "te" (if https? cp encoded) "st") (str "te" cp "st")]
                                     [(str "test" (if https? cp encoded)) (str "test" cp)]]]
        (let [expected (cond
                         (or (== i 0) (and https? (== i 0x1F))) "host"
                         stripped?                               "test"
                         :else                                   expected-part)]
          (is (= (url-string {:scheme scheme :host expected}) (url/href (url/with u k input)))
              (name k))))
      (doseq [[input expected] [[(str cp "9000") (if stripped? "9000" "8000")]
                                [(str "90" cp "00") (if stripped? "9000" "90")]
                                [(str "9000" cp) "9000"]]]
        (is (= (url-string {:scheme scheme :port expected}) (url/href (url/with u :port input)))
            "port"))
      (doseq [k                     [:pathname :search :hash]
              [expected-part input] [[(str encoded "test") (str cp "test")]
                                     [(str "te" encoded "st") (str "te" cp "st")]
                                     [(str "test" encoded) (str "test" cp)]]]
        (is (= (url-string {:scheme scheme k (if stripped? "test" expected-part)})
               (url/href (url/with u k input)))
            (name k)))
      (doseq [input [(str cp "test") (str "te" cp "st") (str "test" cp)]]
        (is (= [[(if stripped? "test" input) ""]]
               (url/search-params (url/with u :search input)))
            "searchParams")))))
