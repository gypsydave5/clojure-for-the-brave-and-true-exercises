(ns fwpd.core)
;; seq is the 'underlying' data structure in Clojure - it allows the polymorphism in functions such as 'map' by converting the data structure into a seq(uence) before hand...
;; behold, we call this...
                                        ; The Sequence Abstraction

(= (seq '(1 2 3)) (seq [1 2 3]))

;; hashmaps get converted into seqs of vector pairs
(= (seq '([:a 1] [:b 2])) (seq {:a 1 :b 2}))

;; this form can be read back into the map using into
(= (into {} (seq '([:a 1] [:b 2]))) {:a 1 :b 2})

                                        ; more map

;; map can apply - obvs
(= '(2 3 4) (map inc [1 2 3]))

;; but multiple collections take each item as an arg (w/o zip)
(= '("Aa" "Bb" "Cc") (map str ["A" "B" "C"] ["a" "b" "c"]))

;; map can take functions too... obviously

(def sum #(reduce + %))
(def avg #(/ (sum %) (count %)))
(defn stats
  [numbers]
  (map #(% numbers) [sum count avg]))

(= [17 3 17/3] (stats [3 4 10]))

;; map can be used like a pluck given the way Clojure keywords work

(def identities
  [{:alias "Batman" :real "Bruce Wayne"}
   {:alias "Spider-Man" :real "Peter Parker"}
   {:alias "Santa" :real "Your mum"}
   {:alias "Easter Bunny" :real "Your dad"}])

(= (map :real identities) '("Bruce Wayne" "Peter Parker" "Your mum" "Your dad"))

                                        ; reduce
;; to map in a hashmap values

(reduce (fn [new-map [key value]]
          (assoc new-map key (inc value)))
        {}
        {:max 30 :min 10})

(defn map-values
  [fun hmap]
  (reduce (fn [new-hmap [key value]]
            (assoc new-hmap key (fun value)))
          {}
          hmap))

(defn map-hashmap
  ([fun hmap]
   (reduce (fn [new-hmap [key value]]
             (assoc new-hmap key (fun value)))
           {}
           hmap))
  ([fval fkey hmap]
   (reduce (fn [new-hmap [key value]]
             (assoc new-hmap (fkey key) (fval value)))
           {}
           hmap)))

(map-values dec {:max 30 :min 10})
(map-hashmap dec {:max 30 :min 10})
(map-hashmap dec str {:max 30 :min 10})

;; can be used as a filter

(reduce (fn [new-map [key val]]
          (if (> val 4)
            (assoc new-map key val)
            new-map))
        {}
        {:human 4.1 :critter 3.0})

                                        ; take, drop, take-while, and drop-while
(= '(1 2 3) (take 3 [1 2 3 4 5 6 7 8]))
(= '(6 7 8) (drop 5 [1 2 3 4 5 6 7 8]))
(= '(1 2 3 4) (take-while #(< % 5) [1 2 3 4 5 6 7 8]))
(= '(6 7 8) (drop-while #(< % 6) [1 2 3 4 5 6 7 8]))

                                        ; filter and some
(= '(2 4 6 8) (filter even? [1 2 3 4 5 6 7 8]))
(= true (some #(= % 8) [1 2 3 4 5 6 7 8]))
(= 8 (some #(and (= % 8) %) [1 1 1 1 1 8 1 1 1 1])) ;; (and) evaluates to the last value

                                        ; sort
(= [1 2 3] (sort [2 1 3]))
(= '("aaa" "bb" "c") (sort '("aaa" "c" "bb")))
(= '("c" "bb" "aaa") (sort-by count '("aaa" "c" "bb")))

                                        ; concat
(= [1 2 3 4] (concat [1 2] [3 4]))

                                        ; lazy seq
(def vampire-database
  {0 {:makes-blood-puns? false :has-pulse true :name "McFishwich"}
   1 {:makes-blood-puns? false :has-pulse true :name "McMackson"}
   2 {:makes-blood-puns? true :has-pulse false :name "Damon Salvatore"}
   3 {:makes-blood-puns? false :has-pulse true :name "Mickey Mouse"}})

(defn vampire-related-details
  [social-security-number]
  (Thread/sleep 10)
  (get vampire-database social-security-number))

(defn vampire?
  [record]
  (and (:makes-blood-puns? record)
       (not (:has-pulse record))
       record))

(defn identify-vampire
  [social-security-numbers]
  (first (filter vampire?
                 (map vampire-related-details social-security-numbers))))

(time (vampire-related-details 0))      ;; about one second
(time (def mapped-details (map vampire-related-details (range 0 100000)))) ;; way less than a second
;; so at this point, mapped-details is unrealised
(time (first mapped-details))           ;; slow the first time...
(realized? mapped-details)
(time (first mapped-details))           ;; ... but memoizes

;; repeat for sexps
(= '("boo" "boo" "boo" "boo" "boo" "boo" "boo" "boo" "boo" "boo")
   (take 10 (repeat "boo")))
;; repeatedly for functions
(= '(10 10 10) (take 3 (repeatedly (fn [] 10))))

;; to infinity...
(defn even-numbers
  ([] (even-numbers 0))
  ([n] (cons n (lazy-seq (even-numbers (+ n 2))))))

(take 10 (even-numbers))
(realized? (take 10 (even-numbers)))

                                        ; The Collection Abstraction
;; one function working on many types of collection
(= true (empty? []) (empty? '()) (empty? #{}))

;; into
(map identity {:sunlight-reaction "Glitter!"}) ;; => ([:sunlight-reaction "Glitter!"])
(into {} (map identity {:sunlight-reaction "Glitter!"})) ;; => {:sunlight-reaction "Glitter!"}

(map identity [:garlic :sesame-oil :fried-eggs])
;; => (:garlic :sesame-oil :fried-eggs)
(into [] (map identity [:garlic :sesame-oil :fried-eggs]))
;; => [:garlic :sesame-oil :fried-eggs]
(into #{} (map identity [:garlic-clove :garlic-clove]))
;; => #{:garlic-clove}
(into {:favorite-emotion "gloomy"} [[:sunlight-reaction "Glitter!"]])
;; => {:favorite-emotion "gloomy", :sunlight-reaction "Glitter!"}
(into ["cherry"] '("pine" "spruce"))
;; => ["cherry" "pine" "spruce"]

;; conj
(conj [0] [1])
;; [0 [1]]
(conj [0] 1)
;; [0 1]
(conj [0] 1 2 3 4 5)
;; [0 1 2 3 4 5]

                                        ; Function Functions
;; apply
(max 0 1 2)
;; 2
(max [0 1 2])
;; [0 1 2]
(apply max [0 1 2])
;; 2

;; partial
(def add10 (partial + 10))
(add10 3)
;; 13
(add10 13)
;; 23

(defn my-partial
  "Always good practice this one"
  [partialized-fn & args]
  (fn [& more-args]
    (apply partialized-fn (into args more-args))))

(def add20 (my-partial + 20))
(add20 3)
;; 23

;; complement
(= true (true? true))
(= false ((complement true?) true))

(defn my-complement
  [fun]
  (fn [& args]
    (not (apply fun args))))

(= false ((my-complement true?) true))
