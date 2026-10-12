(ns dk.simongray.whatwg-url.test-data
  "The files that dev/fetch.sh puts in dev-resources, read the same way on
  the JVM and in Node. Each is nil when it isn't there, and the tests of
  its data then check nothing."
  (:require #?(:cljs ["fs" :as fs])
            #?(:clj [clojure.data.json :as json])
            #?(:clj [clojure.java.io :as io])))

(defn text
  "The text of the file at `path` in dev-resources, or nil."
  [path]
  (let [path (str "dev-resources/" path)]
    #?(:clj  (let [f (io/file path)]
               (when (.exists f) (slurp f)))
       :cljs (when (fs/existsSync path)
               (fs/readFileSync path "utf8")))))

(defn json
  "The data of the JSON file at `path` in dev-resources, with string keys,
  or nil."
  [path]
  (when-let [s (text path)]
    #?(:clj  (json/read-str s)
       :cljs (js->clj (js/JSON.parse s)))))
