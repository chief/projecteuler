(ns euler.primes-test
  (:require [clojure.test :refer :all]
            [euler.primes :refer :all]))

(deftest remove-multiples-of-test
  (testing "Removing multiples of number n from sequence s"
    (is (= [2 3 5 7 9] (remove-multiples-of 2 [2 3 4 5 6 7 8 9 10])))))

(deftest test-sieve-of-eratosthene
  (testing "Getting primes by sieve of eratosthenes"
    (is (= [2 3 5 7] (sieve-of-eratosthenes 10)))))

(deftest factorization-test
  (testing "Getting factorization of numbers"
    (is (= [2 2 2 3 5] (factorization 120)))))
