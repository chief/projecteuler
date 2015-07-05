(ns euler.core-test
  (:require [clojure.test :refer :all]
            [euler.core :refer :all]))

(deftest power-ten-divisor-test
  (testing "Getting maximum power ten divisor"
    (is (= 1000 (power-ten-divisor 7676) ))))

(deftest test-sum-of-tupples
  (testing "Getting all sums of duets"
    (is (= [(+ 1 2) (+ 1 3) (+ 1 4) (+ 2 3) (+ 2 4) (+ 3 4)]
           (sum-of-tupples [1 2 3 4] 2)))))

(deftest test-digit-x-power?
  (testing "Checking whether a number is equals to the sum of its digits
           powered by x"
    (is (= true (digit-x-power? 1 10)))
    (is (= false (digit-x-power? 123 5)))))
