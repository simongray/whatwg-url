(ns dk.simongray.whatwg-url.params
  "The name-value pairs of application/x-www-form-urlencoded, which the
  query of a URL and the body of a form hold, by the URL Standard, section
  5, with the operations of URLSearchParams of section 6.2.

  The pairs are a vector of name-value vectors in order, and a name can
  come more than once. Each function also takes a map or any other seq of
  pairs, e.g. {:q \"clojure\" :page 2}, and reads a keyword as its name,
  e.g. :q as q, and any other value as its text."
  (:refer-clojure :exclude [get set sort])
  (:require [clojure.string :as str]
            [dk.simongray.whatwg-url.chars :as chars]
            [dk.simongray.whatwg-url.percent :as percent]))

(defn- text
  "The text of the name or value `x`."
  [x]
  (if (keyword? x) (subs (str x) 1) (str x)))

;; ClojureScript makes a regular expression for each replace, so it's only
;; done when there's a + to replace
(defn- decode
  "The name or value `s` with + as a space, percent-decoded."
  [s]
  (percent/decode (cond-> s (str/includes? s "+") (str/replace "+" " "))))

;; URL Standard, section 5.1
(defn parse
  "The pairs of the application/x-www-form-urlencoded text `s`, e.g. the
  query of a URL. A + is a space, and a name without = has an empty value.

      (parse \"q=clojure+podcasts&page=2&debug\")
      ;; => [[\"q\" \"clojure podcasts\"] [\"page\" \"2\"] [\"debug\" \"\"]]"
  [s]
  ;; the first = by its index, since ClojureScript splits with a limit by a
  ;; slow loop of re-find
  (into []
        (comp (remove #(= "" %))
              (map #(if-let [eq (str/index-of % "=")]
                      [(decode (subs % 0 eq)) (decode (subs % (inc (long eq))))]
                      [(decode %) ""])))
        (str/split (chars/scalar-values (str s)) #"&")))

;; URL Standard, section 5.2
(defn serialize
  "The application/x-www-form-urlencoded text of the `pairs`, which leaves
  out a pair whose value is nil.

      (serialize {:q \"clojure podcasts\" :page 2})
      ;; => \"q=clojure+podcasts&page=2\""
  [pairs]
  (let [encode #(percent/encode (chars/scalar-values (text %))
                                percent/form-urlencoded-set)]
    ;; one pass into one builder, with no lazy seq between
    (transduce (comp (filter (fn [[_ v]] (some? v)))
                     (map (fn [[k v]] (str (encode k) "=" (encode v))))
                     (interpose "&"))
               (completing chars/append! str)
               (chars/builder)
               pairs)))

(defn- named?
  "Whether the pair of `k` and `v` has the `name`, and the `value` when
  one is given."
  ([name [k]]
   (= (text name) (text k)))
  ([name value [k v]]
   (and (= (text name) (text k)) (= (text value) (text v)))))

(defn get
  "The value of the first pair of `pairs` with the `name`, or nil."
  [pairs name]
  (some (fn [[_ v :as pair]] (when (named? name pair) (text v))) pairs))

(defn get-all
  "The values of each pair of `pairs` with the `name`, in order."
  [pairs name]
  (into [] (comp (filter #(named? name %)) (map (comp text second))) pairs))

(defn has?
  "Whether `pairs` has a pair with the `name`, and the `value` when one is
  given."
  ([pairs name]
   (boolean (some #(named? name %) pairs)))
  ([pairs name value]
   (boolean (some #(named? name value %) pairs))))

(defn append
  "The `pairs` with the pair of `name` and `value` at the end."
  [pairs name value]
  (conj (vec pairs) [name value]))

(defn delete
  "The `pairs` without those that have the `name`, and the `value` when one
  is given."
  ([pairs name]
   (into [] (remove #(named? name %)) pairs))
  ([pairs name value]
   (into [] (remove #(named? name value %)) pairs)))

(defn set
  "The `pairs` with the value of the first pair with the `name` replaced by
  `value`, and the other pairs with the name left out, or with the pair at
  the end when there's none."
  [pairs name value]
  (if (has? pairs name)
    (first (reduce (fn [[out seen?] pair]
                     (cond
                       (not (named? name pair)) [(conj out pair) seen?]
                       seen?                    [out true]
                       :else                    [(conj out [(first pair) value]) true]))
                   [[] false]
                   pairs))
    (append pairs name value)))

(defn sort
  "The `pairs` sorted by name, by UTF-16 code units, with the pairs of a
  name in the order they were in."
  [pairs]
  (vec (sort-by (comp text first) pairs)))

(comment
  (parse "a=b+c&%61=%2B&=x&&y")
  (serialize [["q" "≡ 2"] [:page 2] [:debug nil]])
  (set [["a" "1"] ["b" "2"] ["a" "3"]] "a" "x")
  #_.)
