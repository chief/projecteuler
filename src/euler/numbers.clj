(ns euler.numbers)

; Definition of positive natural numbers
(def natural-numbers (iterate inc 1))

(defn pentagon-number
  "Returns a pentagon number with rank n"
  [n]
  (/ (* n (- (* 3 n) 1)) 2))

(def pentagon-numbers (map #(pentagon-number %) natural-numbers))

(defn triangle-number
  "Returns a triangle number with rank n"
  [n]
  (reduce + (take n natural-numbers)))

(def triangle-numbers (map first (iterate (fn [[a b]] [ (+ a b) (inc b)]) [1 2])))

(defn digits
  "Returns a seq of digits of number n"
  [n]
  (map #(Integer/parseInt (Character/toString %)) (str n)))