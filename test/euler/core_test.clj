(ns euler.core-test
  (:require [clojure.test :refer :all]
            [euler.core :refer :all]))

(deftest power-ten-divisor-test
  (testing "Getting maximum power ten divisor"
    (is (= 1000 (power-ten-divisor 7676)))))

(deftest test-sum-of-tupples
  (testing "Getting all sums of duets"
    (is (= [(+ 1 2) (+ 1 3) (+ 1 4) (+ 2 3) (+ 2 4) (+ 3 4)]
           (sum-of-tupples [1 2 3 4] 2)))))

(deftest test-concatenated-product
  (testing "Getting concatenated-product"
    (is (= 91827 (concatenated-product 9 3)))
    (is (= 192384576 (concatenated-product 192 3)))))
