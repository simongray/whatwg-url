(ns dk.simongray.whatwg-url.tables
  "Write the Unicode tables in resources from the files of the Unicode
  Character Database that dev/fetch.sh downloads.

      clojure -X:tables

  Each table is a run of code points with the same value on each line: the
  gap from the end of the run before, the length and the value, the numbers
  in base 36. Code points that no run covers have the table's default."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def source-dir
  "Where dev/fetch.sh puts the files of the Unicode Character Database."
  "dev-resources/unicode")

(def target-dir
  "Where the tables go."
  "resources/dk/simongray/whatwg_url")

(defn data-lines
  "The fields of each line of the file `name` that isn't a comment."
  [name]
  (for [line  (str/split-lines (slurp (io/file source-dir name)))
        :let  [line (str/trim (first (str/split line #"#" 2)))]
        :when (seq line)]
    (mapv str/trim (str/split line #";"))))

(defn code-points
  "The code points of the range `s`, e.g. 0041..005A."
  [s]
  (let [[a b] (map #(Long/parseLong % 16) (str/split s #"\.\."))]
    (range a (inc (or b a)))))

(defn base-36
  "The number `n` in base 36."
  [n]
  (Long/toString n 36))

(defn sequence-value
  "The code points of the space-separated hexadecimal `s` in base 36,
  separated by dots."
  [s]
  (->> (str/split s #" ")
       (remove str/blank?)
       (map #(base-36 (Long/parseLong % 16)))
       (str/join ".")))

(defn idna-value
  "The value of the code point `c` with the IDNA `status` and `mapping`, or
  nil when it's disallowed, the default."
  [c status mapping]
  (let [cps (map #(Long/parseLong % 16) (remove str/blank? (str/split (str mapping) #" ")))]
    (case status
      "valid"      "v"
      "ignored"    "i"
      "deviation"  (str "d" (sequence-value (str mapping)))
      "mapped"     (if (= 1 (count cps))
                     (str "m" (base-36 (- (first cps) c)))
                     (str "s" (sequence-value mapping)))
      "disallowed" nil)))

(defn idna-values
  "The value of each code point of IdnaMappingTable.txt that isn't
  disallowed, in order."
  []
  (for [[range status mapping] (data-lines "IdnaMappingTable.txt")
        c                      (code-points range)
        :let                   [v (idna-value c status mapping)]
        :when                  v]
    [c v]))

(defn property-values
  "The value of each code point of the property file `name` that `f` gives
  a value for, in order."
  [name f]
  (->> (for [[range v] (data-lines name)
             :let      [v (f v)]
             :when     v
             c         (code-points range)]
         [c v])
       (sort-by first)))

(defn canonical-decomposition
  "The code points of the decomposition field `d` of UnicodeData.txt, or
  nil when it's empty or not canonical."
  [d]
  (when-not (or (str/blank? d) (str/starts-with? d "<"))
    (map #(Long/parseLong % 16) (str/split d #" "))))

(defn non-inert-values
  "Each code point of UnicodeData.txt that NFC doesn't leave alone, in
  order: one with a canonical combining class other than 0, a canonical
  decomposition, or a place in one. The rest are inert, as ICU has it.
  Hangul is left out, since its NFC is computed rather than listed."
  []
  (let [rows  (data-lines "UnicodeData.txt")
        parts (set (mapcat #(canonical-decomposition (nth % 5)) rows))]
    (for [[c _ _ ccc _ d] rows
          :let            [c (Long/parseLong c 16)]
          :when           (or (not= "0" ccc) (canonical-decomposition d) (parts c))]
      [c true])))

(defn runs
  "The runs of consecutive code points with the same value in `values`, as
  triples of the first code point, the last and the value."
  [values]
  (reduce (fn [acc [c v]]
            (let [[a b w] (peek acc)]
              (if (and (= v w) (= c (inc b)))
                (conj (pop acc) [a c v])
                (conj acc [c c v]))))
          []
          values))

(defn table
  "The text of the table of `values`, with a first line that names the
  `source`."
  [source values]
  (->> (runs values)
       (reduce (fn [[lines end] [a b v]]
                 [(conj lines (str/trim (str (base-36 (- a end)) " "
                                             (base-36 (inc (- b a))) " "
                                             (when-not (true? v) v))))
                  (inc b)])
               [[(str "# Unicode 18.0.0, " source)] 0])
       first
       (str/join "\n")))

(defn write!
  "Write each table of the URL library to resources."
  [_]
  (let [tables {"idna.txt"    (table "IdnaMappingTable.txt" (idna-values))
                "bidi.txt"    (table "DerivedBidiClass.txt"
                                     (property-values "DerivedBidiClass.txt"
                                                      #(when (not= "L" %) %)))
                "joining.txt" (table "DerivedJoiningType.txt"
                                     (property-values "DerivedJoiningType.txt"
                                                      #{"L" "D" "R" "T"}))
                "marks.txt"   (table "DerivedGeneralCategory.txt, Mn, Mc and Me"
                                     (property-values "DerivedGeneralCategory.txt"
                                                      #(contains? #{"Mn" "Mc" "Me"} %)))
                "virama.txt"  (table "DerivedCombiningClass.txt, 9"
                                     (property-values "DerivedCombiningClass.txt"
                                                      #(= "9" %)))
                "nfc.txt"     (table "UnicodeData.txt, the code points that NFC doesn't leave alone"
                                     (non-inert-values))}]
    (doseq [[name text] tables]
      (spit (io/file target-dir name) (str text "\n"))
      (println name (count text) "characters"))))
