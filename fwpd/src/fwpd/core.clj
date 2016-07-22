(ns fwpd.core)
(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-keys value]
  ((get conversions vamp-keys) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

                                        ; Exercises

;; 2, 3
(defn append
  [valid suspects new-suspect]
  (if (valid new-suspect)
    (conj suspects new-suspect)
    (do (println "Invalid suspect")
        suspects)))

(defn validate
  [validation-functions map-to-validate]
  (every? (fn [[mkey value]]
            (if (mkey validation-functions)
              ((get validation-functions mkey) value)
              false))
          map-to-validate))

(def append-validate
  (partial append (partial validate
                    {:name #(string? %)
                     :glitter-index #(integer? %)})))

(= [{:name "bob" :glitter-index 5}] (append-validate [] {:name "bob" :glitter-index 5}))
(= [] (append-validate [] {:name "bob" :glittindex 5}))
(= [] (append-validate [] {:name "bob" :glitter-index 5 :foo "bar"}))
(= [{:name "phil" :glitter-inder 10}]
   (append-validate [{:name "phil" :glitter-inder 10}] {:name "bob" :glittindex 5}))

;; 4
(defn record->row
  [separator record]
  (clojure.string/join separator (vals record)))

(defn rows->csv
  [rows]
  (clojure.string/join "\n" rows))

(defn records->csv
  [records]
  (rows->csv (map (partial record->row ",") records)))
