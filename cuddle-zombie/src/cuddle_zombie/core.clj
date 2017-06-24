(ns cuddle-zombie.core)

(def fred (atom {:cuddle-hunger-level 0
                 :percent-deteriorated 0}))

(let [zombie-state @fred]
  (if (>= (:percent-deteriorated zombie-state) 50)
    (future (println (str "Deterioriated: " (:percent-deteriorated zombie-state))))))

(defn increase-cuddle-hunger-level
  [zombie-state increase-by]
  (merge-with + zombie-state {:cuddle-hunger-level increase-by}))

(swap! fred increase-cuddle-hunger-level 5)
(swap! fred update :cuddle-hunger-level + 5)
(reset! fred {:cuddle-hunger-level 0 :percent-deteriorated 0})

;; Watches

(defn shuffle-speed
  [zombie]
  (* (:cuddle-hunger-level zombie)
     (- 100 (:percent-deteriorated zombie))))

(defn shuffle-alert
  [key watched old-state new-state]
  (let [sph (shuffle-speed new-state)]
    (if (> sph 5000)
      (do
        (println "Run, you fool!")
        (println "The zombie's SPH is now" sph)
        (println "This message brought to you courtesy of" key))
      (do
        (println "All's well with" key)
        (println "Cuddle hunger: " (:cuddle-hunger-level new-state))
        (println "Percent deteriorated: " (:percent-deteriorated new-state))
        (println "SPH: " sph)))))

(add-watch fred :fred-shuffle-alert shuffle-alert)

;; Validators

(defn percent-deteriorated-validator
  [{:keys [percent-deteriorated]}]
  (and (>= percent-deteriorated 0)
       (<= percent-deteriorated 100)))

(def bobby
  (atom
   {:cuddle-hunger-level 0 :percent-deteriorated 0}
   :validator percent-deteriorated-validator))

;;;; With a throw

(defn percent-deteriorated-validator
  [{:keys [percent-deteriorated]}]
  (or (and (>= percent-deteriorated 0)
           (<= percent-deteriorated 100))
      (throw (IllegalStateException. "That's not mathsy!"))))

(def harry
  (atom {:cuddle-hunger-level 0 :percent-deteriorated 0}
        :validator percent-deteriorated-validator))

;;;; This would be a lot cooler if I knew how to handle exceptions in Clojure...

;; Refs

(def sock-varieties #{"darned" "argyle" "wool" "horsehair" "mulleted" "passive-aggressive" "striped" "polka-dotted" "athletic" "business" "power" "invisible" "gollumed"})

(defn sock-count
  [sock-variety count]
  {:variety sock-variety
   :count count})

(defn generate-sock-gnome
  "Create an initial sock gnome with no socks"
  [name]
  {:name name
   :socks #{}})

(def sock-gnome (ref (generate-sock-gnome "Barumpharumph")))
(def dryer (ref {:name "LG 1337"
                 :socks (set (map #(sock-count % 2) sock-varieties))}))

(defn steal-sock
  [gnome dryer]
  (dosync

   (when-let [pair (some #(if (= (:count %) 2) %) (:socks @dryer))]
     (let [updated-count (sock-count (:variety pair) 1)]
       (alter gnome update :socks conj updated-count)
       (alter dryer update-in [:socks] disj pair)
       (alter dryer update-in [:socks] conj updated-count)))))

(steal-sock sock-gnome dryer)

(defn similar-socks
  [target-sock sock-set]
  (filter #(= (:variety %) (:variety target-sock)) sock-set))

(defn transaction-demonstration []
  (let [counter (ref 0)
        f (future
            (dosync
             (alter counter inc)
             (println "Value in transaction #1" @counter)
             (Thread/sleep 500)
             (alter counter inc)
             (println "Value in transaction #2" @counter)
             "Finished!"))]
    (Thread/sleep 250)
    (println "Counter unaltered outside of transaction:" @counter)
    (println "Transaction finished - " @f)
    (println "Final value of counter " @counter)))

;; commute

(defn sleep-print-update
  [sleep-time thread-name update-fn]
  (fn [state]
    (Thread/sleep sleep-time)
    (println thread-name ": " state)
    (update-fn state)))

(defn good-commute []
  (let [counter (ref 0)]
    (future (dosync (commute counter (sleep-print-update 100 "Thread A" inc))))
    (future (dosync (commute counter (sleep-print-update 150 "Thread B" inc))))))

(defn bad-commute []
  (let [receiver-a (ref #{})
        receiver-b (ref #{})
        giver (ref #{1})]
    (do (future (dosync
                 (let [gift (first @giver)]
                   (Thread/sleep 10)
                   (commute receiver-a conj gift)
                   (commute giver disj gift))))
        (future (dosync
                 (let [gift (first @giver)]
                   (Thread/sleep 50)
                   (commute receiver-b conj gift)
                   (commute giver disj gift))))
        (Thread/sleep 100)
        (println "@receiver-a" @receiver-a)
        (println "@receiver-b" @receiver-b)
        (println "@giver" @giver))))

;; vars

;;; Dynamic Binding


(def ^:dynamic *notification-address* "dobby@elf.org")

;; *out* is the dynamic var for stdout
(binding [*out* (clojure.java.io/writer "print-output")]
  (println "A man who carries a cat by the tail learns something he can learn in no other way.

-- Mark Twain"))

;; *print-length* specifies how many to print in a clollection

(binding [*print-length* 1]
  (println '("only" "one" "of" "these")))

;; var root

;; meh

;; pmap

(def alphabet-length 26)
;; Vector of chars, A-Z

(def letters (mapv (comp str char (partial + 65)) (range alphabet-length)))

(defn random-string
  [length]
  (apply str (repeatedly length #(rand-nth letters))))

(defn random-string-list
  [list-length string-length]
  (doall (repeatedly list-length (partial random-string string-length))))

(def orc-names (random-string-list 3000 7000))

(defn map-time
  []
  (time (dorun (map clojure.string/lower-case orc-names))))

(defn pmap-time
  []
  (time (dorun (pmap clojure.string/lower-case orc-names))))
