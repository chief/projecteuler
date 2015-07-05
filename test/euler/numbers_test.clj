(ns euler.numbers-test
  (:require [clojure.test :refer :all]
            [euler.numbers :refer :all]))

(deftest naturals-test
  (testing "Getting natural numbers"
    (is (= [1] (take 1 naturals)))))

(deftest pentagon-test
  (testing "Getting pentagon number"
    (is (= 35 (pentagon 5))))
  (testing "Getting pentagon numbers"
    (is (= [1 5 12] (take 3 pentagons)))))

(deftest triangle-test
  (testing "Getting triangle number"
    (is (= 6 (triangle 3))))
  (testing "Getting triangle numbers"
    (is (= [1 3 6] (take 3 triangles)))))

(deftest test-self-power
  (testing "Getting self power number"
    (is (= 256 (self-power 4)))))

(deftest test-quadratic-formula
  (testing "Getting quadratic formula"
    (is (= 33 (quadratic-formula 4 4 1)))))

(deftest factorial-test
  (testing "Getting factorials"
    (is (= 120 (factorial 5)))))

(deftest test-digit-factorial-number?
  (testing "Checking whether a number is equals to the sum of factorial of its
            digits"
    (is (= false (digit-factorial-number? 12)))))

(deftest test-digit-x-power?
  (testing "Checking whether a number is equals to the sum of its digits
           powered by x"
    (is (= true (digit-x-power? 1 10)))
    (is (= false (digit-x-power? 123 5)))))

(deftest test-pandigital?
  (testing "Checking whether a number is pandigital or not"
    (is (= true (pandigital? 987654321)))
    (is (= false (pandigital? 987868)))))
