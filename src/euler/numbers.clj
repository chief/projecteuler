(ns euler.numbers)

; Definition of positive natural numbers
(def naturals (iterate inc 1))

(defn pentagon
  "Returns a pentagon number with rank n"
  [n]
  (/ (* n (- (* 3 n) 1)) 2))

(def pentagon-numbers
  (map #(pentagon %) naturals))

(defn triangle
  "Returns a triangle number with rank n"
  [n]
  (reduce + (take n naturals)))

(def triangle-numbers
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

(defn pandigital-number?
  "Checks if a number is a pandigital number"
  [n]
  (= (sort (digits n)) (range 1 (inc (count (digits n))))))
