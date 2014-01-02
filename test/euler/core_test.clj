(ns euler.core-test
  (:require [clojure.test :refer :all]
            [euler.core :refer :all]))

(deftest euler-digits
  (testing "Rotating euler digits"
    (is (= [123 312 231] (rotate-digits 123)))))

(deftest power-ten-divisor-test
  (testing "Getting maximum power ten divisor"
    (is (= 1000 (power-ten-divisor 7676) ))))

