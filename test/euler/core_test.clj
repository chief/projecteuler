(ns euler.core-test
  (:require [clojure.test :refer :all]
            [euler.core :refer :all]))

(deftest power-ten-divisor-test
  (testing "Getting maximum power ten divisor"
    (is (= 1000 (power-ten-divisor 7676) ))))

(deftest divide?-test
  (testing "Dividing two numbers"
    (is (true? (divide? 2 10)))
    (is (false? (divide? 3 11)))))

(deftest divisors-test
  (testing "Divisors of numbers"
    (is (= [10 1 2 5] (divisors 10)))))

(deftest proper-divisors-test
  (testing "Proper divisors of numbers"
    (is (= [1 2 5] (proper-divisors 10)))))

(deftest factorial-test
  (testing "Getting factorials"
    (is (= 120 (factorial 5)))))

(deftest factorization-test
  (testing "Getting factorization of numbers"
    (is (= [2 2 2 3 5] (factorization 120)))))

(deftest remove-multiples-of-test
  (testing "Removing multiples of number n from sequence s"
    (is (= [2 3 5 7 9] (remove-multiples-of 2 [2 3 4 5 6 7 8 9 10])))))

(deftest test-sieve-of-eratosthene
  (testing "Getting primes by sieve of eratosthenes"
    (is (= [2 3 5 7] (sieve-of-eratosthenes 10)))))

(deftest test-sum-of-divisors
  (testing "Getting sum of divisors"
    (is (= 18 (sum-of-divisors 10)))))

(deftest test-dividable?
  (testing "Checking whether a sequence s is divided by n"
    (is (nil? (dividable? [1 2 3 4 5] 6)))
    (is (true? (dividable? [1 2 3 4] 2)))))

(deftest test-sum-of-duets
  (testing "Getting all sums of duets"
    (is (= [(+ 1 2) (+ 1 3) (+ 1 4) (+ 2 3) (+ 2 4) (+ 3 4)] (sum-of-duets [1 2 3 4])))))

(deftest test-self-power-number
  (testing "Getting self power number"
    (is (= 256 (self-power-number 4)))))

(deftest test-quadratic-formula
  (testing "Getting quadratic formula"
    (is (= 33 (quadratic-formula 4 4 1)))))

(deftest test-my-gcd
  (testing "Getting great common divisor"
    (is (= 6 (my-gcd 12 18)))))

(deftest test-my-lcm
  (testing "Getting least common multiple"
    (is (= 60 (my-lcm 12 10)))))

(deftest test-digit-factorial-number?
  (testing "Checking whether a number is equals to the sum of factorial of its
            digits"
    (is (= false (digit-factorial-number? 12)))))

(deftest test-digit-x-power?
  (testing "Checking whether a number is equals to the sum of its digits
           powered by x"
    (is (= true (digit-x-power? 1 10)))
    (is (= false (digit-x-power? 123 5)))))
