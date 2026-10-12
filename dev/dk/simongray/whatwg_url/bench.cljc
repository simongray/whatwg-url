(ns dk.simongray.whatwg-url.bench
  "Benchmarks of a URL of each kind that a page links to, on the JVM by
  criterium and in Node by the clock, and flame graphs of the JVM by
  clj-async-profiler.

      clojure -M:bench -m dk.simongray.whatwg-url.bench
      clojure -M:cljs release bench && node target/bench.js

  In a REPL with the :bench alias, (run) gives the mean of each case in
  microseconds, and (profile :absolute 10) the file of a flame graph."
  (:require #?@(:clj [[clj-async-profiler.core :as prof]
                      [criterium.core :as criterium]])
            [dk.simongray.whatwg-url :as url]
            [dk.simongray.whatwg-url.hosts :as hosts]
            [dk.simongray.whatwg-url.params :as params]))

(def page
  "The URL of the page that the links are resolved against."
  "https://example.com/blog/2026/10/post.html")

(def cases
  "Each case by name, as a function to time."
  (let [base (url/parse page)
        link "https://www.example.com/blog/2026/10/post.html?page=2#comments"
        u    (url/parse link)]
    {:normal   #(url/resolve base link)
     :absolute #(url/parse link)
     :upper    #(url/resolve base "HTTPS://www.Example.com/blog/2026/10/post.html?page=2#comments")
     :relative #(url/resolve base "../images/photo.jpg")
     :idn      #(url/parse "https://bücher.example/")
     :ipv4     #(url/parse "http://0x7f.1/")
     :ipv6     #(url/parse "http://[2001:db8:0:0:1:0:0:1]/")
     :unicode  #(hosts/domain->unicode "xn--bcher-kva.xn--fa-hia.example")
     :href     #(url/href u)
     :errors   #(url/validation-errors "https://user@example.org/a b")
     :with     #(url/with page :pathname "/v1/search" :search-params {:q "clojure podcasts" :page 2})
     :params   #(params/parse "q=clojure+podcasts&page=2&sort=new")}))

#?(:cljs
   (defn- mean
     "The mean time of a call of `f` in seconds, over a second of calls
     after a second of warming up."
     [f]
     (let [now   #(js/performance.now)
           calls (fn [ms]
                   (let [end (+ (now) ms)]
                     (loop [n 0]
                       (if (< (now) end)
                         (do (dotimes [_ 100] (f)) (recur (+ n 100)))
                         n))))]
       (calls 1000)
       (let [start (now)
             n     (calls 1000)]
         (/ (- (now) start) n 1000)))))

(defn run
  "The mean time of each case, or of those named `ks`, in microseconds."
  ([]
   (run (keys cases)))
  ([ks]
   (into (sorted-map)
         (for [k    ks
               :let [mean #?(:clj  (first (:mean (criterium/quick-benchmark* (cases k) {})))
                             :cljs (mean (cases k)))]]
           [k (/ (Math/round (* mean 1e9)) 1000.0)]))))

#?(:clj
   (defn profile
     "The file of a flame graph of the case `k`, run for `seconds` by
     clj-async-profiler."
     [k seconds]
     (let [f   (cases k)
           end (+ (System/currentTimeMillis) (* 1000 seconds))]
       (prof/profile (while (< (System/currentTimeMillis) end) (f))))))

(defn -main
  "Print the mean time of each case."
  [& _]
  (doseq [[k us] (run)]
    (println (str (name k) (apply str (repeat (- 10 (count (name k))) " ")) us " µs")))
  #?(:clj (shutdown-agents)))

(comment
  (run)
  (run [:absolute :relative])
  (profile :absolute 10)
  #_.)
