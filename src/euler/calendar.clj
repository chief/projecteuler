(ns euler.calendar)

(defn leap-year
  [n]
  (if (= (mod n 100) 0)
    (= (mod n 400) 0)
    (= (mod n 4) 0)))


(defn month-durations
  [n]
  (if (leap-year n)
    [31 29 31 30 31 30 31 31 30 31 30 31]
    [31 28 31 30 31 30 31 31 30 31 30 31]))

(defn starting-month-days
  ([y]
    (starting-month-days y 0))
  ([y offset]
    (loop [s offset x [0] m (month-durations y)]
      (if (empty? m)
        [x s]
        (let [v (+ s (first m))]
          (recur v (conj x v) (rest m)))))))

(defn month-days-period
  [from to]
  (loop [year from days [] offset 0]
    (if (> year to)
      (distinct (flatten days))
      (let [[d off] (starting-month-days year offset)]
        (recur (inc year) (conj days d) off)))))
