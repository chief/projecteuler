(ns euler.division
  (:require [clojure.math.numeric-tower :as numeric-tower]
            [clojure.spec :as s]))

(defn divide?
  "Checks if number x divides n"
  [x n]
  (zero? (rem n x)))

(s/fdef divide?
        :args (s/cat :start integer? :end integer?)
        :ret boolean?)

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

(s/fdef divisors
        :args (s/cat :n integer?)
        :ret seq?
        :fn #(= (first (:ret %)) (:n %))  )

(defn proper-divisors
  "Gets proper divisors of number n

   https://en.wikipedia.org/wiki/Divisor"
  [n]
  (rest (divisors n)))

(defn sum-of-divisors
  "Sum of divisors for number n"
  [n]
  (reduce + (divisors n)))

(defn dividable?
  "Checks whether any item of a sequence s is divided by number n"
  [s n]
  (some #(true? (divide? n %)) s))

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
