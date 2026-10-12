(ns ^:no-doc dk.simongray.whatwg-url.unicode
  "The properties of Unicode 18.0.0 that IDNA needs: the IDNA mapping
  table of UTS #46, the bidi classes, the joining types, the marks and the
  viramas, and the platform's NFC.

  The tables in resources are made by dev/dk/simongray/whatwg_url/tables.clj
  from the Unicode Character Database, and are read on first use.

  The platform normalizes to NFC by the version of Unicode it has. That's
  the NFC of Unicode 18.0.0 too, since NFC never changes for the code
  points that a version has and leaves most new ones alone (e.g. a new
  emoji). The exception is a code point that the platform lacks and NFC
  doesn't leave alone, which known? looks for.

  TODO: carry the NFC of Unicode 18.0.0 instead, if domains with such code
  points turn up, e.g. with a combining mark of a new script. Then no
  platform would refuse them."
  #?(:cljs (:require-macros [dk.simongray.whatwg-url.unicode :refer [table-text]]))
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            [dk.simongray.whatwg-url.chars :as chars])
  #?(:clj (:import [java.text Normalizer Normalizer$Form])))

#?(:clj
   ;; ClojureScript carries the table as a string literal, which the JVM
   ;; can't, as a constant is at most 64 KB there
   (defmacro table-text
     "The text of the table `name` in resources, as a string literal in
     ClojureScript, and read when it's evaluated on the JVM."
     [name]
     (let [path (str "dk/simongray/whatwg_url/" name)]
       (if (:ns &env)
         (slurp (io/resource path))
         `(slurp (io/resource ~path))))))

(defn- base-36
  "The number in the text `s`, read in base 36."
  ^long [s]
  #?(:clj (Long/parseLong s 36) :cljs (js/parseInt s 36)))

;; arrays rather than vectors, so that a lookup compares primitive numbers
(defn read-table
  "The runs of the `text` of a table, as an array of the first code points,
  an array of the last code points and an array of what `f` returns for
  the value of each run."
  [text f]
  (let [lines  (vec (rest (str/split-lines text)))
        n      (count lines)
        firsts (long-array n)
        lasts  (long-array n)
        values (object-array n)]
    (loop [k 0 end 0]
      (if (< k n)
        (let [[gap len v] (str/split (nth lines k) #" ")
              a           (+ end (base-36 gap))
              b           (+ a -1 (base-36 len))]
          (aset firsts k a)
          (aset lasts k b)
          (aset values k (f v))
          (recur (inc k) (inc b)))
        [firsts lasts values]))))

(defn lookup
  "The value of the code point `c` in the `table` that read-table returns,
  or nil when no run has it."
  [[^longs firsts ^longs lasts ^objects values :as table] ^long c]
  (loop [lo 0 hi (dec (alength firsts))]
    (when (<= lo hi)
      (let [mid (quot (+ lo hi) 2)]
        (cond
          (< c (aget firsts mid)) (recur lo (dec mid))
          (< (aget lasts mid) c)  (recur (inc mid) hi)
          :else                   (aget values mid))))))

(defn- sequence-value
  "The text of the code points in base 36, separated by dots, of `s`."
  [s]
  (if (= "" s)
    ""
    (chars/from-code-points (map base-36 (str/split s #"\.")))))

(defn- idna-value
  "The status and mapping of the value `v` of the IDNA table."
  [v]
  (case (subs v 0 1)
    "v" [:valid]
    "i" [:ignored ""]
    "d" [:deviation (sequence-value (subs v 1))]
    "s" [:mapped (sequence-value (subs v 1))]
    "m" [:mapped (base-36 (subs v 1))]))

(def idna-table
  "The IDNA mapping table of UTS #46, read on first use."
  (delay (read-table (table-text "idna.txt") idna-value)))

(defn idna-status
  "The status of the code point `c` by the IDNA mapping table, and its
  mapping when it's ignored, mapped or a deviation, as a pair, e.g.
  [:mapped \"a\"] for U+0041. A code point the table disallows is
  [:disallowed]."
  [^long c]
  ;; the table's own value, rather than a new vector, unless its mapping
  ;; is an offset from the code point
  (if-let [[status m :as value] (lookup @idna-table c)]
    (if (number? m)
      [status (chars/from-code-point (+ c (long m)))]
      value)
    [:disallowed]))

(def bidi-table
  "The bidi classes other than L, read on first use."
  (delay (read-table (table-text "bidi.txt") keyword)))

(defn bidi-class
  "The bidi class of the code point `c` as a keyword, e.g. :L or :AL."
  [c]
  (or (lookup @bidi-table c) :L))

(def joining-table
  "The joining types L, D, R and T, read on first use."
  (delay (read-table (table-text "joining.txt") keyword)))

(defn joining-type
  "The joining type of the code point `c`, :L, :D, :R or :T, or nil for
  another."
  [c]
  (lookup @joining-table c))

(def mark-table
  "The code points of the general category Mark, read on first use."
  (delay (read-table (table-text "marks.txt") (constantly true))))

(defn mark?
  "Whether the code point `c` is a mark, of the general category Mn, Mc or
  Me."
  [c]
  (some? (lookup @mark-table c)))

(def virama-table
  "The code points of the canonical combining class Virama, read on first
  use."
  (delay (read-table (table-text "virama.txt") (constantly true))))

(defn virama?
  "Whether the code point `c` has the canonical combining class Virama."
  [c]
  (some? (lookup @virama-table c)))

(def nfc-table
  "The code points that NFC doesn't leave alone, read on first use."
  (delay (read-table (table-text "nfc.txt") (constantly true))))

;; Unicode, section 3.12, for Hangul, which UnicodeData.txt doesn't list
(defn inert?
  "Whether NFC leaves the code point `c` alone in any text, as ICU has it:
  its canonical combining class is 0, and it has no canonical
  decomposition and no place in one."
  [^long c]
  (not (or (lookup @nfc-table c)
           (<= 0x1100 c 0x1112)
           (<= 0x1161 c 0x1175)
           (<= 0x11A8 c 0x11C2)
           (<= 0xAC00 c 0xD7A3))))

#?(:cljs
   (def unassigned
     "The code points that the JavaScript engine doesn't have."
     (js/RegExp. "\\p{Cn}" "u")))

(defn- defined?
  "Whether the platform's Unicode has the code point `c`."
  [c]
  #?(:clj  (Character/isDefined (int c))
     :cljs (not (.test unassigned (js/String.fromCodePoint c)))))

(defn known?
  "Whether the platform's NFC of the text `s` is that of Unicode 18.0.0,
  i.e. whether the platform has each code point of `s` that isn't inert.
  Code points that the IDNA table disallows don't count, since they make a
  domain fail anyway."
  [s]
  (or #?(:cljs (not (.test unassigned s)))
      (let [n (chars/length s)]
        (loop [i 0]
          (or (== i n)
              (let [c (chars/code-point-at s i)]
                (and (or (defined? c) (inert? c) (= [:disallowed] (idna-status c)))
                     (recur (+ i (chars/width c))))))))))

(defn nfc
  "The text `s` in Unicode Normalization Form C, by the platform."
  [s]
  #?(:clj  (Normalizer/normalize s Normalizer$Form/NFC)
     :cljs (.normalize s "NFC")))
