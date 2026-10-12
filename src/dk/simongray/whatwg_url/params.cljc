(ns dk.simongray.whatwg-url.params
  "The name-value pairs of application/x-www-form-urlencoded, which the
  query of a URL and the body of a form hold, by the URL Standard, section
  5, with the operations of URLSearchParams of section 6.2.

  The pairs are a vector of name-value vectors, in order, a name that comes
  twice twice. Each function also takes a map or any other seq of pairs,
  e.g. {:q \"clojure\" :page 2}, and reads a keyword as its name without
  the colon and any other value as its text."
  (:refer-clojure :exclude [get set sort])
  (:require [clojure.string :as str]
            [dk.simongray.whatwg-url.chars :as chars]
            [dk.simongray.whatwg-url.percent :as percent]))

(defn- text
  "The text of the name or value `x`."
  [x]
  (if (keyword? x) (subs (str x) 1) (str x)))

(defn- decode
  "The name or value `s` with + as a space, percent-decoded."
  [s]
  (percent/decode (str/replace s "+" " ")))

;; URL Standard, section 5.1
(defn parse
  "The pairs of the application/x-www-form-urlencoded text `s`, e.g. the
  query of a URL. A + is a space, and a name without = has an empty value.

      (parse \"q=clojure+podcasts&page=2&debug\")
      ;; => [[\"q\" \"clojure podcasts\"] [\"page\" \"2\"] [\"debug\" \"\"]]"
  [s]
  (into []
        (comp (remove #(= "" %))
              (map #(let [[k v] (str/split % #"=" 2)]
                      [(decode k) (decode (or v ""))])))
        (str/split (chars/scalar-values (str s)) #"&")))

;; URL Standard, section 5.2
(defn serialize
  "The application/x-www-form-urlencoded text of the `pairs`, which leaves
  out a pair whose value is nil.

      (serialize {:q \"clojure podcasts\" :page 2})
      ;; => \"q=clojure+podcasts&page=2\""
  [pairs]
  (->> (for [[k v] pairs
             :when (some? v)]
         (str (percent/encode (chars/scalar-values (text k)) percent/form-urlencoded-set)
              "="
              (percent/encode (chars/scalar-values (text v)) percent/form-urlencoded-set)))
       (str/join "&")))

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
