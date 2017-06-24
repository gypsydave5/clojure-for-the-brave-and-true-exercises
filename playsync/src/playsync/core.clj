(ns playsync.core
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread alts! alts!! timeout]]))

;; the hot dog machine
(defn hot-dog-machine
  []
  (let [in (chan)
        out (chan)]
    (go (<! in)
        (>! out "hot dog"))
    [in out]))

;; using the hot dog machine
(let [[in out] (hot-dog-machine)]
  (>!! in "pocket lint")
  (<!! out))

;; a significantly better hotdog machine
(defn hot-dog-machine-v2
  [hot-dog-count]
  (let [in (chan)
        out (chan)]
    (go (loop [hc hot-dog-count]
          (if (not (zero? hc))
            (let [input (<! in)]
              (if (= 3 input)
                (do (>! out "hot dog")
                    (recur (dec hc)))
                (do (>! out "wilted lettuce")
                    (recur hc))))
            (do (close! in)
                (close! out)))))
    [in out]))

(let [[in out] (hot-dog-machine-v2 2)]
  (>!! in "pocket lint")
  (println (<!! out))

  (>!! in 3)
  (println (<!! out))

  (>!! in 3)
  (println (<!! out))

  (>!! in 3)
  (println (<!! out)))

(defn reversy-upcasey-with-channels
  [string]
  (let [c1 (chan)
        c2 (chan)
        c3 (chan)]
    (go (>! c2 (clojure.string/upper-case (<! c1))))
    (go (>! c3 (clojure.string/reverse (<! c2))))
    (go (println (<! c3)))
    (>!! c1 string)))

; alts!!
;; alts!! takes an array of channels and returns the first one that gives a value
;; along with that value

(defn upload
  [headshot c]
  (go (Thread/sleep (rand 100))
      (>! c headshot)))

(defn upload-headshots
  []
  (let [c1 (chan)
        c2 (chan)
        c3 (chan)]
    (upload "serious.jpg" c1)
    (upload "fun.jpg" c2)
    (upload "sassy.jpg" c3)
    (let [[headshot channel] (alts!! [c1 c2 c3])]
      (println "Sending headshot notification for" headshot)
      channel)))

(defn timeout-uploads
  []
  (let [c1 (chan)]
    (upload "serious.jpg" c1)
    (let [[headshot channel] (alts!! [c1 (timeout 20)])]
      (if headshot
        (println "sending headshot notification for" headshot)
        (println "Timed out!")))))

;; queues

(defn append-to-file
  "Write a string to the end of a file"
  [filename s]
  (spit filename s :append true))

(defn format-quote
  "Delineate the beginning and end of a quote because it's convenient"
  [quote]
  (str "=== BEGIN QUOTE ===\n" quote "=== END QUOTE ===\n\n"))

(defn random-quote
  "Retrieve a random quote and format it"
  []
  (format-quote (slurp "http://www.braveclojure.com/random-quote")))

(defn snag-quotes
  [filename number-of-quotes]
  (let [c (chan)]
    (go (while true (append-to-file filename (<! c))))
    (dotimes [_ number-of-quotes] (go (>! c (random-quote))))))

;; process pipelines

(defn upper-caser
  [in]
  (let [out (chan)]
    (go (while true (>! out (clojure.string/upper-case (<! in)))))
    out))

(defn reverser
  [in]
  (let [out (chan)]
    (go (while true (>! out (clojure.string/reverse (<! in)))))
    out))

(defn printer
  [in]
  (go (while true (println (<! in)))))


(defn process-string
  [s]
  (let [in-chan (chan)
        upper-caser-out (upper-caser in-chan)
        reverser-out (reverser upper-caser-out)]
    (printer reverser-out)
    (>!! in-chan s)
    (>!! in-chan s)))
