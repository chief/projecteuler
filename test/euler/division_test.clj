(ns euler.division-test
  (:require [clojure.test :refer :all]
            [euler.division :refer :all]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]))

(deftest test-divide?
  (testing "Dividing two numbers"
    (is (true? (divide? 2 10)))
    (is (false? (divide? 3 11)))))

(deftest test-divisors
  (testing "Divisors of numbers"
    (is (= [10 1 2 5] (divisors 10)))))

(deftest test-proper-divisors
  (testing "Proper divisors of numbers"
    (is (= [1 2 5] (proper-divisors 10)))))

(deftest test-sum-of-divisors
  (testing "Getting sum of divisors"
    (is (= 18 (sum-of-divisors 10)))))

(deftest test-dividable?
  (testing "Checking whether a sequence s is divided by n"
    (is (nil? (dividable? [1 2 3 4 5] 6)))
    (is (true? (dividable? [1 2 3 4] 2)))))

(deftest test-my-gcd
  (testing "Getting great common divisor"
    (is (= 6 (my-gcd 12 18)))))

(deftest test-my-lcm
  (testing "Getting least common multiple"
    (is (= 60 (my-lcm 12 10)))))
