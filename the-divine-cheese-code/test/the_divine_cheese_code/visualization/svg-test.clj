(ns the-divine-cheese-code.visualization.svg-test
  (:require [clojure.test :refer :all])
  (:require [the-divine-cheese-code.visualization.svg :refer :all]))

(deftest test-max
  (testing "max function returns minimum of a vector of coordinate maps"
    (is (=  (max [{:lng 6.1 :lat 55}
                  {:lng 8.9 :lat 0.1}]) {:lat 55 :lng 8.9}))))
(deftest test-min
  (testing "min function returns minimum of a vector of coordinate maps"
    (is (=  (min [{:lng 6.1 :lat 55}
                  {:lng 8.9 :lat 0.1}]) {:lat 0.1 :lng 6.1}))))















