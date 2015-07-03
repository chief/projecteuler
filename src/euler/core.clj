(ns euler.core
  (:require [clojure.math.combinatorics :as combo]
            [euler.numbers :as numbers]
            [clojure.math.numeric-tower :as numeric-tower])
  (:use [clojure.set :refer [difference]]
        [criterium.core]
        [clojure.math.combinatorics]
        [clojure.java.io :only (reader)]
        [euler.utils]))

(defn divide?
  "Checks if number x divides n"
  [x n]
  (= (rem n x) 0))

(defn divisors
  "Gets divisors of number n.

   First number is n second number is 1 followed by other divisors - if exist."
  [n]
  (loop [divisors [n 1] x 2]
    (if (> (* x x) n)
      (distinct divisors)
      (if (divide? x n)
        (recur (conj divisors x (/ n x)) (inc x))
        (recur divisors (inc x))))))

(defn factorial
  "Calculates the factorial (n!) of number n"
  [n]
  (reduce *' (take n numbers/naturals)))

(defn proper-divisors
  "Gets proper divisors of number n

   https://en.wikipedia.org/wiki/Divisor"
  [n]
  (rest (divisors n)))

(defn remove-multiples-of
  "Given a number n and a sequence s, removes all multiples of number n that
   belong to s"
  [n s]
  (if (= n 1)
    s
    (remove #(and (divide? n %) (not (= n %))) s)))

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
      (let [divisor (first (filter #(divide? % x) primes))]
        (if (nil? divisor)
          []
          (let [result (/ x divisor)]
            (if (= result 1)
              (conj factors divisor)
              (recur result (conj factors divisor)))))))))

(defn sum-of-divisors
  "Sum of divisors for number n"
  [n]
  (reduce + (divisors n)))


(defn dividable?
  "Checks whether any item of a sequence s is divided by number n"
  [s n]
  (some #(true? (divide? n %)) s))


(defn sum-of-duets
  "Retuns the sum of all duets (no duplication) in a sequence s"
  [s]
  (map #(reduce + %) (combinations s 2)))

(defn self-power-number
  "Returns the self power of a number n"
  [n]
  (numeric-tower/expt n n))

(defn quadratic-formula
  "Applies quadratic formula in number n given a and b coefficients"
  [n a b]
  (+ (* n n ) (* a n) b))

(defn my-gcd
  "Gets great common divisor of two numbers

   https://en.wikipedia.org/wiki/Euclidean_algorithm"
  [x y]
  (if (= x y)
    x
    (if (< x y)
      (recur x (- y x))
      (recur (- x y) y))))

(defn my-lcm
  "Gets least common multiple.

   https://en.wikipedia.org/wiki/Least_common_multiple"
  [x y]
  (/ (numeric-tower/abs (* x y)) (my-gcd x y)))

(defn digit-factorial-number?
  "Checks if a number n is equal to the sum of the factorial of its digits"
  [n]
  (= (reduce + (map factorial (numbers/digits n))) n))

(defn digit-x-power?
  "Checks if a number n is equal to the sum of its digits powered by x"
  [n x]
  (= (int (reduce + (map #(Math/pow % x) (numbers/digits n)))) n))

;
; Untested code ****************************************************************
;


(defn prime-number?
  "Checks if a number is a prime number given a known set of primes"
  [n primes]
  (not (= (.indexOf primes n) -1)))

(defn count-quadratic-formulas
  "Counts the succesive times of a quadratic formula"
  [a b primes]
  (loop [x 0]
    (if (prime-number? (quadratic-formula x a b) primes)
      (recur (inc x))
      { [a b] x })))

(defn examine-quadratic-formulas
  [max-a max-b]
  (let [primes (sieve-of-eratosthenes 100000)]
    (map (fn [[a b]] (count-quadratic-formulas a b primes))
      (filter (fn [[a b]] (and (odd? a) (odd? b) (> (+ a b 1) 0)))
        (for [x (range (- max-a) max-a)
              y (range 2 max-b)] [x y])))))





(defn powers
  "Returns all powers of number n between 2 <= b <= (max power)"
  [n max-power]
  (map #(numeric-tower/expt n %) (range 2 (inc max-power))))

(defn sequence-of-powers
  [max-n max-power]
  (reduce (fn [x y]
            (distinct (apply conj x y)))
  (map #(powers % max-power) (range 2 (inc max-n)))))

(defn number-permutations
  "Returns all permutations of a number n with the same number of digits that
   are greater from n"
  [n]
  (remove #(= n %)
    (filter #(and (= (numbers/count-of-digits %) (numbers/count-of-digits n)) (> % n))
      (map #(Integer/parseInt(apply str %)) (permutations (numbers/digits n))))))

(defn prime-number-permutations
  "Returns prime number permutations given a number n"
  [n primes]
  (sort (filter #(prime-number? % primes) (number-permutations n))))

(defn number-sequence
  "Given a number n and a sequence s returns a sequence (if exists) with fixed
   step starting with number n"
   [n s]
   (first
    (map #(vector n % (+ % (- % n)))
    (filter (fn [a]
              (let [d (- a n)
                    examine-prime (+ a d)]
                (not (= (.indexOf s examine-prime) -1))))
      s))))

(defn digits-to-integer
  "Returns an integer from a sequence of digits"
  [s]
  (Integer/parseInt (apply str s)))

(defn circular-numbers
  "Returns all circular numbers of number n"
  [n]
    (loop [x n circulars []]
      (let [d (numbers/digits x)
            next-num (digits-to-integer (conj (vec (rest d)) (first d)))]
        (if (or (= next-num x) (= next-num n))
          circulars
          (recur next-num (conj circulars next-num))))))

(defn impossible-circular-prime?
  "Checks if a number cannot be a circular prime by its digits"
  [n]
  (if (and (> n 10) (re-find #"[0|2|4|6|8]" (str n)))
    true
    false))

(defn circular-prime?
  "Checks if a prime is a circular."
  [n primes]
  (every? #(prime-number? % primes) (circular-numbers n)))

(defn left-truncate-numbers
  "Returns all numbers by truncate the digits of number n from left to right"
  [n]
    (let [d (numbers/digits n)]
      (loop [x d numbers []]
        (let [r (rest x)]
          (if (empty? r)
            numbers
            (recur r (conj numbers (digits-to-integer r))))))))

(defn right-truncate-numbers
  "Returns all numbers by truncate the digits of number n from right to left"
  [n]
   (let [d (vec (numbers/digits n))]
      (loop [x d numbers []]
        (let [r (pop x)]
          (if (empty? r)
            numbers
            (recur r (conj numbers (digits-to-integer r))))))))


(defn truncate-numbers
  "Returns left and right truncate numbers for number n"
  [n]
  (distinct (flatten (conj (right-truncate-numbers n) (left-truncate-numbers n)))))

(defn truncatable-prime?
  "Checks if a prime number is truncatable or not"
  [n primes]
  (every? #(prime-number? % primes) (sort (truncate-numbers n))))

(defn circular-primes
  "Returns all circular primes below number n"
  [n]
  (let [primes (sieve-of-eratosthenes n)]
    (filter #(and (not (impossible-circular-prime? %))
                  (circular-prime? % primes))
     primes)))

(defn lattice-paths
  [x y]
  (if (and (> x 1) (> y 1))
    (+ (lattice-paths (dec x) y) (lattice-paths x (dec y)))
    1))

(def memo-lattice-paths (memoize lattice-paths))

(defn max-product-array
  [array]
  (reduce * (map #(array %) (take 4 (iterate inc 0)))))

(defn value-of-word
  "Returns value of a word by summing each letter ranking in the english alphabet"
  [n]
  (reduce + (map #(inc (.indexOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (str %))) (vec n))))

(defn summary-criterion
  "Given an ordered sequence s containing positive numbers and a positive number
   n checks if n can b written as a sum of two numbers from s."
  [s, n]
  (if (<= n (first s))
    false
    (or (not (= (.indexOf s (/ n 2)) -1))
        (let [new-sequence (filter #(< % n) s)]
          (boolean
            (some #(>= (.indexOf new-sequence %) 0)
              (map #(- n %) new-sequence)))))))

(defn ackermann
  "Computes the Ackermann function

   https://en.wikipedia.org/wiki/Ackermann_function"
   [x y]
   (cond
      (= y 0) 0
      (= x 0) (* 2 y)
      (= y 1) 2
      :else
        (ackermann (dec x) (ackermann x (dec y)))))

(defn amicable-pair
  "Given a number returns its amicable pair or nil"
  [n]
  (let [result (sum-of-divisors n)]
    (if (and (not (= result n)) (= (sum-of-divisors result) n))
      result
      nil)))

(defn amicable?
  "Checks if a number is amicable"
  [n]
  (not (= (amicable-pair n) nil)))


(defn abundant-number?
  "Checks if a number is an abundant number.

   For more information visit https://en.wikipedia.org/wiki/Abundant_number"
  [n]
  (> (reduce + (proper-divisors n)) n))

(defn abundant-numbers
  "Take abundant numbers up to n"
  [n]
  (filter abundant-number? (take n numbers/naturals)))

(defn deficient-number?
  "Checks if a number is a deficient number"
  [n]
  (< (reduce + (proper-divisors n)) n))

(defn perfect-number?
  "Checks if a number is a perfect number"
  [n]
  (= (reduce + (proper-divisors n)) n))


;
; Experimental code
;
;
(defn differences
  "Given a sequence of numbers returns a sequence of differences"
  [s])

(defn rotate-digits
  "Returns digit rotations for a given number n"
  [n]
  )

(defn power-ten-divisor
  "Returns the maximum power ten divisor"
  [n]
  (numeric-tower/expt 10 (dec (count (numbers/digits n)))))

(defn consecutive-prime-sum
  "Examines the count of consecutive prime sum that results to a prime"
  [n primes]
  (loop [x 2 results []]
    (let [subject (reduce + (take x primes))]
      (if (> subject 1000000)
        results)

      (if (prime-number? subject primes)
        (recur (inc x) (conj results subject))
        (recur (inc x) results)))))
