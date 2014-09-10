(ns euler.core-test
  (:require [clojure.test :refer :all]
            [euler.core :refer :all]))

(deftest power-ten-divisor-test
  (testing "Getting maximum power ten divisor"
    (is (= 1000 (power-ten-divisor 7676) ))))

(deftest divisors-test
  (testing "Getting divisors of a nnumber"
    (is (= '(1 10 2 5) (divisors-v4 10)))))