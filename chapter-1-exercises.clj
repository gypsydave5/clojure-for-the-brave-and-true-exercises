;; 1. Use str, vector, list, hash-map and hash-set functions.

(= (str 5) "5")
(= (vector 5 6 7 8) [5 6 7 8])
(= (list "a" :b 3) '("a" :b 3))
(= (hash-map :a 1 :b 2) {:a 1 :b 2})
(= (hash-set :a :a :a :b :b) #{:a :b})

;; 2. Write a function that takes a number and adds 100 to it.
(defn add-one-hundred [x] (+ x 100))

;; 3 Write a function, dec-maker, that works exactly like the
;; function inc-maker except with subtraction

(defn dec-maker
  [dec-by]
  #(- % dec-by))

((dec-maker 3) 10)

;; 4. Write a function, mapset, that works ike map except returns
;; the value as a set

(defn map-set
  [fn ls]
  (set (map fn ls)))

(map-set inc [1 1 2 2])
