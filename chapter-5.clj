;; is pure
(defn wisdom
  [words]
  (str words ", Daniel-san"))
;; ain't pure
(defn year-end-evaluation
  []
  (if (> (rand) 0.5)
    "You get a raise!"
    "Better luck next year!"))

                                        ; Living with Immutable Data Structures

;; functional alternative to iteration is recursion

;; new values can only be associated with names in new scopes - like `let`
(def great-baby-name "Rosandthony")
(let [great-baby-name "Bloodthunder"]
  great-baby-name)

;; recursion! list-processing! car! cdr!
(defn sum-tc
  ([vals] (sum vals 0))
  ;; nice way to slip a default value in...
  ([vals accumulating-total]
   (if (empty? vals)
     accumulating-total

     (sum (rest vals)
          (+ (first vals) accumulating-total)))))

;; ah! no TCO - I <3 you Scheme! - so we're stuck with

(defn sum-recur
  ([vals] (sum vals 0))

  ([vals accumulating-total]
   (if (empty? vals)
     accumulating-total

     (recur (rest vals)
            (+ (first vals) accumulating-total)))))

;; which isn't all that bad...
;; see http://hypirion.com/musings/understanding-persistent-vector-pt-1 for how Clojure
;; makes all the new copies of data structures work without going crazy. It _is_ kinda
;; like Git :D

(require '[clojure.string :as s])
(defn clean
  [text]
  (s/replace (s/trim text) #"lol" "LOL"))

;; Cool stuff with pure functions
(#(< 4 %) 5)
;; comp

((comp #(< 4 %) inc *) 2 3)

(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})
(def c-int (comp :intelligence :attributes))
(def c-str (comp :strength :attributes))
(def c-dex (comp :dexterity :attributes))

(c-int character) ;; => 10
(c-str character) ;; => 4
(c-dex character) ;; => 5

;; multi-arity
(defn spell-slots
  [char]
  (int (inc (/ (c-int char) 2))))

;; same with comp
(def spell-slots-comp (comp int inc #(/ % 2) c-int))
(= (spell-slots character) (spell-slots-comp character))

;; my-comp
(defn my-comp
  [& fns]
  (fn [& args]
    (loop [fns fns args args]
      (if (empty? (butlast fns))
        (apply (last fns) args)
        (recur (butlast fns) [ (apply (last fns) args)])))))

;; another-my-comp
(defn another-my-comp
  [& fns]
  (fn [& args]
    (reduce (fn [result func] (func result))
            (apply (last fns) args)
            (rest (reverse fns)))))

;; yet-another-my-comp
(defn yet-another-my-comp
  [& fns]
  (fn [& args]
    (loop [fns (reverse fns)
           result (apply (first fns) args)]
      (if (empty? fns)
        result
        (recur (rest fns) ((first fns) result))))))

(= ((my-comp inc inc inc *) 5 5)
   ((comp inc inc inc *) 5 5)
   ((another-my-comp inc inc inc *) 5 5)
   ((yet-another-my-comp inc inc inc *) 5 5))

;; memoization

(defn sleepy-identity
  "Returns the given value after one second"
  [x]
  (Thread/sleep 1000) x)

(sleepy-identity "Mr. Fantastico")

(def memo-sleepy-identity (memoize sleepy-identity))

(memo-sleepy-identity "Mr. Fantastico")
(memo-sleepy-identity "Dr. Worm")
