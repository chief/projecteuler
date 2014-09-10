(ns euler.core
  (:require [clojure.math.combinatorics :as combo])
  (:use [clojure.set :refer [difference]]
        [criterium.core]
        [clojure.math.combinatorics]
        [clojure.java.io :only (reader)]
        [clojure.math.numeric-tower]
        [euler.numbers]
        [euler.utils]))

(defn divide?
  "Checks if number x divides n"
  [x n]
  (= (rem n x) 0))

; FIXME
(defn remove-multiples-of
  "Given a sequence s and a number n, removes all multiples of number n that
   belong to s"
  [n s]
  (if (= n 1)
    s
    (remove #(and (divide? n %) (not (= n %))) s)))

; FIXME
(defn remove-all-multiples-of
  "Given a sequence s and a number n, removes all multiples of number n that
   belong to s and n itself"
  [n s]
  (remove #(= n %) (remove-multiples-of n s)))

(defn multiples-of
  "Gets all multiples between [n, x]"
  [n x]
  (if (nil? x)
    '()
    (take-while #(<= % x) (iterate #(+ n %) n))))

(defn divisors
  "Gets divisors of number n"
  [n]
  (loop [divisors [n 1] x 2]
    (if (> (* x x) n)
      (distinct divisors)
      (if (divide? x n)
        (recur (conj divisors x (/ n x)) (inc x))
        (recur divisors (inc x))))))


(defn proper-divisors
  "Gets proper divisors of number n

   https://en.wikipedia.org/wiki/Divisor"
  [n]
  (rest (divisors n)))

(def seq-of-triangle-numbers
  (map first (iterate (fn [[a b]] [ (+ a b) (inc b)]) [1 2])))

(defn factorial
  [n]
  (reduce *' (take n natural-numbers)))

(defn sieve-of-eratosthenes
  [n]
  (loop [primes (range 2 (inc n)) position 0]
    (let [current-prime (nth primes position)]
      (if (> (Math/pow current-prime 2) (last primes))
        primes
        (recur (remove-multiples-of current-prime primes) (inc position))))))

(defn dividable?
  [s n]
  (> (count (filter #(= (mod % n) 0) s)) 0))

(def few-primes [2 3 5 7 9 11 13 17 19])

(defn find-smaller-divider
  [s primes]
  (let [x (first primes)]
    (if (dividable? s x)
      x
      (if (empty? primes)
        nil
        (find-smaller-divider s (rest primes))))))

(defn alter-on-perfect-division
  [s n]
  (map #(if (= (mod % n) 0) (/ % n) %) s))

(defn lcd
  ([s]
    (lcd s []))
  ([s n]
    (if (= (count s) (apply + s))
      (apply * n)
      (let [divider (find-smaller-divider s few-primes)]
        (if (nil? divider)
          (println "fail")
        (lcd (alter-on-perfect-division s divider) (conj n divider)))))))

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

(defn collatz-number
  "Returns a collatz number"
  [n]
  (if (even? n)
    (/ n 2)
    (+ (* 3 n) 1)))

(defn collatz-sequence
  "Returns a Collatz sequence given a starting number n"
  [n]
  (loop [x n numbers []]
    (let [collatz (collatz-number x)]
      (if (= 1 collatz)
        (conj numbers 1)
        (recur collatz (conj numbers collatz))))))

(defn sum-of-divisors
  "Sum of divisors for number n."
  [n]
  (reduce + (divisors n)))

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

(defn prime-number?
  "Checks if a number is a prime number given a known set of primes"
  [n primes]
  (not (= (.indexOf primes n) -1)))

(defn perfect-number?
  "Checks if a number is a perfect number"
  [n]
  (= (reduce + (proper-divisors n)) n))

(defn deficient-number?
  "Checks if a number is a deficient number"
  [n]
  (< (reduce + (proper-divisors n)) n))

(defn abundant-number?
  "Checks if a number is an abundant number.

   For more information visit https://en.wikipedia.org/wiki/Abundant_number"
  [n]
  (> (reduce + (proper-divisors n)) n))

(defn sum-of-duets
  [s]
  (map #(reduce + %) (combinations s 2)))

(defn abundant-numbers
  "Take abundant numbers up to n"
  [n]
  (filter abundant-number? (take n natural-numbers)))

(defn count-of-digits
  "Returns the number of digits of number n"
  [n]
  (count (str n)))

(def fibonacci-numbers
  (map first (iterate (fn [[a b]] [ (+' a b) a]) [1 0])))

(defn quadratic-formula
  "Applies quadratic formula in number n given a and b coefficients"
  [n a b]
  (+ (* n n ) (* a n) b))

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

(defn read-numbers
  "Reads numbers from a file"
  [filename]
  (map bigint (line-seq (reader filename))))

(defn digit-factorial-number?
  "Check if a number n is equal to the sum of the factorial of its digits"
  [n]
  (= (reduce + (map factorial (digits n))) n))

(defn digit-fifth-power?
  "Checks if a number n is equal to the sum of fifths of its digits"
  [n]
  (= (int (reduce + (map #(Math/pow % 5) (digits n)))) n))

(defn self-power-number
  "Returns the self power of a number n"
  [n]
  (expt n n))

(defn pandigital-number?
  "Checks if a number is a pandigital number"
  [n]
  (= (sort (digits n)) (range 1 (inc (count (digits n))))))

(defn powers
  "Returns all powers of number n between 2 <= b <= (max power)"
  [n max-power]
  (map #(expt n %) (range 2 (inc max-power))))

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
    (filter #(and (= (count-of-digits %) (count-of-digits n)) (> % n))
      (map #(Integer/parseInt(apply str %)) (permutations (digits n))))))

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
      (let [d (digits x)
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
    (let [d (digits n)]
      (loop [x d numbers []]
        (let [r (rest x)]
          (if (empty? r)
            numbers
            (recur r (conj numbers (digits-to-integer r))))))))

(defn right-truncate-numbers
  "Returns all numbers by truncate the digits of number n from right to left"
  [n]
   (let [d (vec (digits n))]
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
  (expt 10 (dec (count (digits n)))))

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
