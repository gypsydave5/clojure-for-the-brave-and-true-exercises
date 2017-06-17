(ns asyncing-feeling.core
  (:require [clj-http.client :as client]))

;; Delay
(def jackson-5-delay
  (delay (let [message "Just call my name and I'll be there"]
           (println "First deref:" message)
           message)))

(def gimli-headshots ["serious.jpg" "fun.jpg" "playful.jpg"])
(defn email-user
  [email-address]
  (println "Sending headshot notification to" email-address))
(defn upload-document
  "Needs to be implemented"
  [headshot]
  true)
(defn upload-and-notify
  [headshots user-email-address]
  (let [notify (delay (email-user user-email-address))]
    (doseq [headshot headshots]
      (future (upload-document headshot)
              (force notify)))))

;; Promise
(def yak-butter-international
  {:store "Yak Butter International"
   :price 90
   :smoothness 90})
(def butter-than-nothing
  {:store "Butter Than Nothing"
   :price 150
   :smoothness 83})
(def baby-got-yak
  {:store "Baby Got Yak"
   :price 94
   :smoothness 99})
(def yak-butters [yak-butter-international butter-than-nothing, baby-got-yak])

(defn mock-api-call
  [result]
  (Thread/sleep 1000)
  result)

(defn satisfactory?
  "If the butter meets our criteria, return the butter, else return false"
  [butter]
  (and (<= (:price butter) 100)
       (>= (:smoothness butter) 97)
       butter))

(defn synchronous-yak-call
  []
  (some (comp satisfactory? mock-api-call) yak-butters))

(defn asynchronous-yak-call
  []
  (let [butter-promise (promise)]
    (doseq [butter yak-butters]
      (future (if-let [satisfactory-butter (satisfactory? (mock-api-call butter))]
                (deliver butter-promise satisfactory-butter))))
    @butter-promise))

(defn callback-style
  []
  (let [ferengi-wisdom-promise (promise)]
    (future (println "Here's some Ferengi wisdom:" @ferengi-wisdom-promise))
    (Thread/sleep 300)
    (deliver ferengi-wisdom-promise "Whisper your way to success.")
    nil))

;; My First Queue
(defmacro wait
  "Sleep `timeout` seconds before evaluating body"
  [timeout & body]
  `(do (Thread/sleep ~timeout) ~@body))

(defmacro enqueue
  ([q concurrent-promise-name concurrent serialized]
   `(let [~concurrent-promise-name (promise)]
      (future (deliver ~concurrent-promise-name ~concurrent))
      (deref ~q)
      ~serialized
      ~concurrent-promise-name))
  ([concurrent-promise-name concurrent serialized]
   `(enqueue (future) ~concurrent-promise-name ~concurrent ~serialized)))

(defn queue-example
  []
  (-> (enqueue saying (wait 200 "'Ello, gov'na!") (println @saying))
      (enqueue saying (wait 400 "Pip pip!") (println @saying))
      (enqueue saying (wait 100 "Cheerio!") (println @saying))))

;; Exercises

;; 1
(defn searchy
  [term]
  (let [google "https://google.com/search?q="
        bing "https://bing.com/search?q="
        result (promise)]
    (future (deliver result (slurp (str google term))))
    (future (deliver result (slurp (str bing term))))
    @result))

;; 2
(defn searchy-2
  [term engines]
  (let [result (promise)]
    (doseq [search engines]
      (future (deliver result (slurp (search term)))))
    @result))

(def engines
  [#(str "https://google.com/search?q=" %)
   #(str "https://bing.com/search?q=" %)])

;; 3

(defn extract-links
  "Returns a lazy sequence of all absolute link urls extracted from a string"
  [s]
  (map second (re-seq #"href=\"(http[^\"]*)\"" s)))

(defn searchy-3
  [term engines]
  (let [result (promise)]
    (doseq [search engines]
      (future (deliver result (slurp (search term)))))
    (extract-links @result)))
