(ns pegthing.core-test
  (:require [clojure.test :refer :all]
            [pegthing.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest test-tri*
  (testing "tri* creates a lazy list of triangular numbers."
    (is (= '(1 3 6) (take 3 (tri*))))
    (is (= '(1 3 6 10) (take 4 (tri*))))))

(deftest tri-test
  (testing "tri is the definition of that list"
    (is (= '(1 3 6 10) (take 4 tri)))))

(deftest test-triangular?
  (testing "triangular? is a predicate test for a triangular number"
    (is (true? (triangular? 6)))
    (is (true? (triangular? 91)))
    (is (false? (triangular? 13)))))

(deftest test-row-tri
  (testing "returns the triangular number at the end of a row"
    (is (= 3 (row-tri 2)))
    (is (= 15 (row-tri 5)))))


(deftest test-row-num
  (testing "row-num returns the row that a number apperas in."
    (is (= 2 (row-num 2)))
    (is (= 4 (row-num 7)))
    (is (= 5 (row-num 14)))))

(deftest test-connect
  (testing "connect connects two positions, constructing a board"
    (is (= {1 {:connections {4 2}} 4 {:connections {1 2}}}
           (connect {} 6 1 2 4))))
  (testing "will not connect out of the bounds"
    (is (= {} (connect {} 3 3 6 10)))))

(deftest test-in-bounds?
  (testing "is the first number the greatest number"
    (is (true? (in-bounds? 15 1 2 4)))
    (is (false? (in-bounds? 15 1 2 22)))))

(deftest test-connect-right
  (testing "connects a position to the one-over neighbour to the right"
    (is (= {4 {:connections {6 5}}
            6 {:connections {4 5}}}
           (connect-right {} 15 4))))
  (testing "will not jump over a triangular number"
    (is (= {} (connect-right {} 15 5)))
    (is (= {} (connect-right {} 15 10)))))

(deftest test-connect-down-left
  (testing "connects a position to the one-over, down and to the left"
    (is (= {1 {:connections {4 2}}
            4 {:connections {1 2}}}
           (connect-down-left {} 15 1))))
  (testing "won't break the bounds"
    (is (= {} (connect-down-left {} 6 4)))))

(deftest test-connect-down-right
  (testing "connects a position to the one-over, down and to the right"
    (is (= {4 {:connections {13 8}}
            13 {:connections {4 8}}}
           (connect-down-right {} 15 4)))))

(deftest test-add-pos
  "generates a peg and its (max) three connections"
  (is (= {1 {:pegged true, :connections {6 3, 4 2}}
          4 {:connections {1 2}}
          6 {:connections {1 3}}}
         (add-pos {} 15 1)))
  (is (= {4 {:pegged true, :connections {6 5, 11 7, 13 8}}
          6 {:connections {4 5}}
          11 {:connections {4 7}}
          13 {:connections {4 8}}}
         (add-pos {} 15 4))))

(deftest test-new-board
  "creates a new board with the correct number of rows"
  (is (= {:rows 1
          1 {:pegged true}} (new-board 1)))
  (is (= {:rows 2
          1 {:pegged true}
          2 {:pegged true}
          3 {:pegged true}} (new-board 2)))
  (is (= {:rows 3
        1 {:pegged true :connections {4 2, 6 3}}
        2 {:pegged true}
        3 {:pegged true}
        4 {:pegged true :connections {1 2, 6 5}}
        5 {:pegged true}
          6 {:pegged true :connections {1 3, 4 5}}} (new-board 3))))

(deftest test-pegged?
  (testing "predicate for position being pegged"
    (is (true? (pegged? {1 {:pegged true}} 1)))
    (is (false? (pegged? {1 {:pegged true}} 2)))))

(deftest test-remove-peg
  (testing "removes the peg at a position"
    (is (= {1 {:pegged false}} (remove-peg {1 {:pegged true}} 1)))))

(deftest test-place-peg
  (testing "places the peg at a position"
    (is (= {1 {:pegged true}} (place-peg {1 {:pegged false}} 1)))))

(deftest test-move-peg
  (testing "moving a peg"
    (is (= {1 {:pegged true} 55 {:pegged false}}
           (move-peg {1 {:pegged false} 55 {:pegged true}} 55 1)))))

(deftest test-valid-moves
  (testing "returns a list of valid moves from a board position"
     (let [board (assoc-in (new-board 5) [4 :pegged] false)]
       (is (= {4 2} (valid-moves board 1)))
       (is (= {4 2 6 3} (valid-moves (remove-peg board 6) 1)))
       (is (= {4 5} (valid-moves board 6)))
       (is (= {4 7} (valid-moves board 11))))))

(deftest test-valid-move?
  (testing "can tell if a move is valid"
    (let [board (assoc-in (new-board 5) [4 :pegged] false)]
      (is (= 2 (valid-move? board 1 4)))
      (is (= nil (valid-move? board 1 6)))
      (is (= 3 (valid-move? (remove-peg board 6) 1 6))))))

(deftest test-make-move
  (testing "makes a move if the move is valid"
    (let [board (assoc-in (new-board 5) [5 :pegged] false)]
      (is (= nil
             (make-move board 1 6)))
      (is (= (place-peg (remove-peg (remove-peg board 12) 8) 5)
             (make-move board 12 5)))
      (is (= (place-peg (remove-peg (remove-peg board 14) 9) 5)
             (make-move board 14 5))))))

(deftest test-can-move?
  (testing "are there any valid moves on the board"
    (let [board (new-board 5)]
      (is (nil? (can-move? board))))
    (let [board (assoc-in (new-board 5) [5 :pegged] false)]
      (is ((complement nil?) (can-move? board))))
    (let [board (assoc-in (new-board 3) [2 :pegged] false)]
      (is (nil? (can-move? board))))))

(deftest test-colorize
  (testing "colorizes a string based on a symbol"
    (is (= "\033[34mblue\033[0m" (colorize "blue" :blue)))
    (is (= "\033[31mred\033[0m" (colorize "red" :red)))
    (is (= "\033[32mgreen\033[0m" (colorize "green" :green)))))
