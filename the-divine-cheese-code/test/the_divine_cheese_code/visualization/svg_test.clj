(ns the-divine-cheese-code.visualization.svg-test
  (:refer-clojure :exclude [min max])
  (:require [clojure.test :refer :all])
  (:require [the-divine-cheese-code.visualization.svg :refer :all]))

(deftest latlng->point-test
  (testing "converts correctly, lng first and then lat"
    (is (= (latlng->point {:lat 1 :lng 2}) "2,1"))))

(deftest points-test
  (testing "converts an empty vector to an empty string"
    (= (points []) ""))
  (testing "converts a list of coordinates into a comma separated list"
    (is (points [{:lat 1 :lng 2} {:lat 3 :lng 4}]) "2,1 4,3")))

(deftest test-max
  (testing "max function returns minimum of a vector of coordinate maps"
    (is (=  (max [{:lng 6.1 :lat 55}
                  {:lng 8.9 :lat 0.1}]) {:lat 55 :lng 8.9}))))
(deftest test-min
  (testing "min function returns minimum of a vector of coordinate maps"
    (is (=  (min [{:lng 6.1 :lat 55}
                  {:lng 8.9 :lat 0.1}]) {:lat 0.1 :lng 6.1}))))

(deftest translate-to-00-test
  (testing "zeroes a list with a single coordinate"
    (is (= [{:lng 0 :lat 0}] (translate-to-00 [{:lat 55 :lng 66}]))))
  (testing "reduces a longer list of coordinates by the smallest values"
    (is (= [{:lat 5 :lng 0}, {:lat 0 :lng 4}]
           (translate-to-00 [{:lat 9 :lng 4} {:lat 4 :lng 8}])))))

(deftest scale-test
  (testing "scales a single coordinate to the given height and width values"
    (is (= [{:lat 9 :lng 5}] (scale 5 9 [{:lat 99 :lng 55}]))))
  (testing "scales other coordinates relative to the ratio"
    (is (= [{:lat 3 :lng 10} {:lat 6 :lng 5}]
           (scale 10 6 [{:lat 1 :lng 2} {:lat 2 :lng 1}])))))
