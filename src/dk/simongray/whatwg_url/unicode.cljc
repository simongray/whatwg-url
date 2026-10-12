(ns dk.simongray.whatwg-url.unicode
  "The properties of Unicode 18.0.0 that IDNA needs: the IDNA mapping
  table of UTS #46, the bidi classes, the joining types, the marks and the
  viramas, and the platform's NFC.

  The tables in resources are made by dev/dk/simongray/whatwg_url/tables.clj from
  the Unicode Character Database. ClojureScript carries them as strings,
  which are read on first use.

  The platform normalizes to NFC, by the version of Unicode it has. NFC
  doesn't change for the code points a version has, so it's the NFC of
  Unicode 18.0.0 unless the text has a code point the platform doesn't
  have, which known? tells."
  #?(:cljs (:require-macros [dk.simongray.whatwg-url.unicode :refer [resource]]))
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            [dk.simongray.whatwg-url.chars :as chars])
  #?(:clj (:import [java.text Normalizer Normalizer$Form])))

#?(:clj
   ;; ClojureScript carries the table as a string literal, which the JVM
   ;; can't, as a constant is at most 64 KB there
   (defmacro ^:no-doc resource
     "The text of the table `name` in resources."
     [name]
     (slurp (io/resource (str "dk/simongray/whatwg_url/" name)))))

#?(:clj
   (defn- table-text
     "The text of the table `name` in resources."
     [name]
     (slurp (io/resource (str "dk/simongray/whatwg_url/" name)))))

(defn ^:no-doc read-table
  "The runs of the `text` of a table, as a vector of the first code points,
  a vector of the last code points and a vector of the values that `f`
  gives the value of each run."
  [text f]
  (loop [lines (rest (str/split-lines text))
         end   0
         firsts (transient [])
         lasts  (transient [])
         values (transient [])]
    (if-let [line (first lines)]
      (let [[gap n v] (str/split line #" ")
            a         (+ end #?(:clj (Long/parseLong gap 36) :cljs (js/parseInt gap 36)))
            b         (+ a -1 #?(:clj (Long/parseLong n 36) :cljs (js/parseInt n 36)))]
        (recur (rest lines) (inc b)
               (conj! firsts a) (conj! lasts b) (conj! values (f v))))
      [(persistent! firsts) (persistent! lasts) (persistent! values)])))

(defn ^:no-doc lookup
  "The value of the code point `c` in the `table` that read-table gives, or
  nil when no run has it."
  [[firsts lasts values] ^long c]
  (loop [lo 0 hi (dec (count firsts))]
    (when (<= lo hi)
      (let [mid (quot (+ lo hi) 2)]
        (cond
          (< c (long (nth firsts mid))) (recur lo (dec mid))
          (< (long (nth lasts mid)) c)  (recur (inc mid) hi)
          :else                         (nth values mid))))))

(defn- sequence-value
  "The text of the code points in base 36, separated by dots, of `s`."
  [s]
  (if (= "" s)
    ""
    (chars/from-code-points
     (map #?(:clj #(Long/parseLong % 36) :cljs #(js/parseInt % 36))
          (str/split s #"\.")))))

(defn- idna-value
  "The status and mapping of the value `v` of the IDNA table."
  [v]
  (case (subs v 0 1)
    "v" [:valid]
    "i" [:ignored ""]
    "d" [:deviation (sequence-value (subs v 1))]
    "s" [:mapped (sequence-value (subs v 1))]
    "m" [:mapped #?(:clj (Long/parseLong (subs v 1) 36) :cljs (js/parseInt (subs v 1) 36))]))

(def ^:no-doc idna-table
  "The IDNA mapping table of UTS #46, read on first use."
  (delay (read-table #?(:clj (table-text "idna.txt") :cljs (resource "idna.txt"))
                     idna-value)))

(defn idna-status
  "The status of the code point `c` by the IDNA mapping table, and its
  mapping when it's ignored, mapped or a deviation, as a pair, e.g.
  [:mapped \"a\"] for U+0041. A code point the table disallows is
  [:disallowed]."
  [^long c]
  (if-let [[status m] (lookup @idna-table c)]
    [status (if (number? m) (chars/from-code-points [(+ c (long m))]) m)]
    [:disallowed]))

(def ^:no-doc bidi-table
  "The bidi classes other than L, read on first use."
  (delay (read-table #?(:clj (table-text "bidi.txt") :cljs (resource "bidi.txt"))
                     keyword)))

(defn bidi-class
  "The bidi class of the code point `c` as a keyword, e.g. :L or :AL."
  [c]
  (or (lookup @bidi-table c) :L))

(def ^:no-doc joining-table
  "The joining types L, D, R and T, read on first use."
  (delay (read-table #?(:clj (table-text "joining.txt") :cljs (resource "joining.txt"))
                     keyword)))

(defn joining-type
  "The joining type of the code point `c`, :L, :D, :R or :T, or nil for
  another."
  [c]
  (lookup @joining-table c))

(def ^:no-doc mark-table
  "The code points of the general category Mark, read on first use."
  (delay (read-table #?(:clj (table-text "marks.txt") :cljs (resource "marks.txt"))
                     (constantly true))))

(defn mark?
  "Whether the code point `c` is a mark, of the general category Mn, Mc or
  Me."
  [c]
  (some? (lookup @mark-table c)))

(def ^:no-doc virama-table
  "The code points of the canonical combining class Virama, read on first
  use."
  (delay (read-table #?(:clj (table-text "virama.txt") :cljs (resource "virama.txt"))
                     (constantly true))))

(defn virama?
  "Whether the code point `c` has the canonical combining class Virama."
  [c]
  (some? (lookup @virama-table c)))

#?(:cljs
   (def ^:no-doc unassigned
     "The code points that the JavaScript engine doesn't have."
     (js/RegExp. "\\p{Cn}" "u")))

(defn known?
  "Whether the platform has each code point of the text `s` in its version
  of Unicode, or has it as unassigned in Unicode 18.0.0 too."
  [s]
  #?(:clj  (every? #(or (Character/isDefined (int %))
                        (= [:disallowed] (idna-status %)))
                   (chars/code-points s))
     :cljs (or (not (.test unassigned s))
               (every? #(or (not (.test unassigned (js/String.fromCodePoint %)))
                            (= [:disallowed] (idna-status %)))
                       (chars/code-points s)))))

(defn nfc
  "The text `s` in Unicode Normalization Form C, by the platform."
  [s]
  #?(:clj  (Normalizer/normalize s Normalizer$Form/NFC)
     :cljs (.normalize s "NFC")))
