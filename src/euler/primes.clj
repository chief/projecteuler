(ns euler.primes
  (:require [euler.division :as division]))

(defn remove-multiples-of
  "Given a number n and a sequence s, removes all multiples of number n that
   belong to s"
  [n s]
  (if (= n 1)
    s
    (remove #(and (division/divide? n %) (not= n %)) s)))

(defn sieve-of-eratosthenes
  "Calculate prime numbers up to number n

   https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes"
  [n]
  (loop [primes (range 2 (inc n)) position 0]
    (let [current-prime (nth primes position)]
      (if (> (Math/pow current-prime 2) (last primes))
        primes
        (recur (remove-multiples-of current-prime primes) (inc position))))))

(defn factorization
  "Integer factorization to primes"
  [n]
  (let [primes (sieve-of-eratosthenes (inc (/ n 2)))]
    (loop [x n factors []]
      (let [divisor (first (filter #(division/divide? % x) primes))]
        (if (nil? divisor)
          []
          (let [result (/ x divisor)]
            (if (= result 1)
              (conj factors divisor)
              (recur result (conj factors divisor)))))))))
