(ns pegthing.core
  (require [clojure.set :as set])
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

;; board set up
(defn tri*
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  "Is the number triangular?"
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn row-tri
  "The triangular number at the end of a row"
  [n]
  (last (take n tri)))

(defn row-num
  "Returns the row number of a position"
  [n]
  (inc (count (take-while #(> n %) tri))))

(defn in-bounds?
  [n & rest]
  (= n (apply max n rest)))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbour destination]
  (if (in-bounds? max-pos pos neighbour destination)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbour))
            board
            [[pos destination] [destination pos]])
    board))

(defn connect-right
  "Connect a position to the neighbour plus one to the right"
  [board max-pos pos]
  (let [neighbour (inc pos)
        destination (inc neighbour)]
    (if-not (or (triangular? neighbour) (triangular? pos))
      (connect board max-pos pos neighbour destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbour (+ row pos)
        destination (+ 1 row neighbour)]
    (connect board max-pos pos neighbour destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbour (+ 1 row pos)
        destination (+ 2 row neighbour)]
    (connect board max-pos pos neighbour destination)))

(defn add-pos
  "Pegs the position and performs connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Creates a new board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

;; peg manipulation
(defn pegged?
  "Predicate for if a board position has a peg in it."
  [board pos]
  (true? (get-in board [pos :pegged])))

(defn remove-peg
  "Removes peg from board at position"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the board at given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take a peg out of p1 and place is in p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn valid-moves
  "Returns a map of all the valid moves from a position"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]] (and (not (pegged? board destination))
                                                (pegged? board jumped)))
                (get-in board [pos :connections]))))

(defn valid-move?
  "Returns nil for invalid moves, the jumped peg for valid moves"
  [board from-pos to-pos]
  (get (valid-moves board from-pos) to-pos))

(defn make-move
  "Returns the new board state if move is valid, nil otherwise"
  [board from-pos to-pos]
  (if-let [jumped (valid-move? board from-pos to-pos)]
    (move-peg (remove-peg board jumped) from-pos to-pos)))

(defn positions-with-pegs
  [board]
  (map first (filter #(get (second %) :pegged) board)))

(defn can-move?
  "are there any more moves left?"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (positions-with-pegs board)))

;; board rendering
(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)
(def ansi-escape "\033[")
(def ansi {:blue (str ansi-escape "34m")
           :red (str ansi-escape "31m")
           :green (str ansi-escape "32m")
           :reset (str ansi-escape "0m")})

(defn colorize
  "Wraps string in ANSI color code escapes"
  [string color]
  (cond (= color :blue) (str (ansi :blue) string (ansi :reset))
        (= color :red) (str (ansi :red) string (ansi :reset))
        (= color :green) (str (ansi :green) string (ansi :reset))
        :else string))

(defn render-pos
  "Creates a string from a board position, converting the position number
  to a letter in the range a-z, with the pegged state as the next character:
  red '-' for no peg, blue '0' for pegged"
  [board pos]
  (str (nth letters (dec pos))
       (if (pegged? board pos)
         (colorize "0" :blue)
         (colorize "-" :red))))

(defn row-positions
  "Returns all the positions on a given row"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))

(defn row-padding
  "String of spaces to leftpad a row"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " "
                            (map (partial render-pos board)
                                 (row-positions row-num)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
