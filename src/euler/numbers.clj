(ns euler.numbers
  (:require [euler.division :as division]
            [clojure.math.numeric-tower :as numeric-tower]))

; Definition of positive natural numbers
(def naturals (iterate inc 1))

(defn pentagon
  "Returns a pentagon number with rank n

  https://en.wikipedia.org/wiki/Pentagonal_number"
  [n]
  (/ (* n (- (* 3 n) 1)) 2))

(def pentagons
  (map pentagon naturals))

(defn triangle
  "Returns a triangle number with rank n"
  [n]
  (reduce + (take n naturals)))

(def triangles
  (map first (iterate (fn [[a b]] [ (+ a b) (inc b)]) [1 2])))

(defn collatz
  "Returns a collatz number"
  [n]
  (if (even? n)
    (/ n 2)
    (+ (* 3 n) 1)))

(defn collatzs
  "Returns a Collatz sequence given a starting number n"
  [n]
  (loop [x n numbers []]
    (let [collatz (collatz x)]
      (if (= 1 collatz)
        (conj numbers 1)
        (recur collatz (conj numbers collatz))))))

(defn digits
  "Returns a seq of digits of number n"
  [n]
  (map #(Integer/parseInt (Character/toString %)) (str n)))

(defn count-of-digits
  "Returns the number of digits of number n"
  [n]
  (count (str n)))

(def fibonacci-numbers
  (map first (iterate (fn [[a b]] [ (+' a b) a]) [1 0])))

(defn pandigital?
  "Checks if a number is a pandigital number"
  [n]
  (= (sort (digits n)) (range 1 (inc (count (digits n))))))

(defn amicable-pair
  "Given a number returns its amicable pair or nil"
  [n]
  (let [result (division/sum-of-divisors n)]
    (if (and (not (= result n)) (= (division/sum-of-divisors result) n))
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
  (> (reduce + (division/proper-divisors n)) n))

(defn abundant-numbers
  "Take abundant numbers up to n"
  [n]
  (filter abundant-number? (take n naturals)))

(defn deficient-number?
  "Checks if a number is a deficient number"
  [n]
  (< (reduce + (division/proper-divisors n)) n))

(defn perfect-number?
  "Checks if a number is a perfect number"
  [n]
  (= (reduce + (division/proper-divisors n)) n))

(defn self-power
  "Returns the self power of a number n"
  [n]
  (numeric-tower/expt n n))

(defn quadratic-formula
  "Applies quadratic formula in number n given a and b coefficients"
  [n a b]
  (+ (* n n ) (* a n) b))

(defn factorial
  "Calculates the factorial (n!) of number n"
  [n]
  (reduce *' (take n naturals)))

(defn digit-factorial-number?
  "Checks if a number n is equal to the sum of the factorial of its digits"
  [n]
  (= (reduce + (map factorial (digits n))) n))

(defn powers
  "Returns a vector with all powers of number n between 2 <= b <= (max power)"
  [n max-power]
  (map #(numeric-tower/expt n %) (range 2 (inc max-power))))
