(ns euler.utils)

(use '[clojure.java.io :only (reader)])
(require '[clojure.string :as str])

(defn parse-numbers
  [file]
  (map #(Integer/parseInt %)
    (flatten
      (map #(clojure.string/split  % #"\s+")
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

(defn parse-words
  [filename]
  (clojure.string/split
    (clojure.string/replace (line-seq (reader filename))  #"[\"|\\|\)|\()]" "")
    #","))

; 48477312 51267216 48477312