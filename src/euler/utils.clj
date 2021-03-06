(ns euler.utils
  (:use [clojure.java.io :only (reader)])
  (:require [clojure.string :as str]))

(defn parse-numbers
  [file]
  (map #(Integer/parseInt %)
       (flatten
        (map #(str/split  % #"\s+")
             (line-seq (reader file))))))

(defn take-all-horizontal
  [s n]
  (loop [x s r []]
    (if (empty? x)
      r
      (recur (rest x) (conj r (take n x))))))

(defn take-all-vertical
  [s n k]
  (loop [x s r []]
    (if (empty? x)
      r
      (recur (rest x) (conj r (take n (flatten (partition 1 k x))))))))

(defn read-numbers
  "Reads numbers from a file"
  [filename]
  (map bigint (line-seq (reader filename))))

(defn parse-words
  [filename]
  (str/split
   (str/replace (line-seq (reader filename))  #"[\"|\\|\)|\()]" "")
   #","))
