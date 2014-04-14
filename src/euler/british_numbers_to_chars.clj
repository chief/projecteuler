(ns euler.british_numbers_to_chars)

(def numbers
{
  0 "zero"
  1 "one"
  2 "two"
  3 "three"
  4 "four"
  5 "five"
  6 "six"
  7 "seven"
  8 "eight"
  9 "nine"
  10 "ten"
  11 "eleven"
  12 "twelve"
  13 "thirteen"
  14 "fourteen"
  15 "fifteen"
  16 "sixteen"
  17 "seventeen"
  18 "eighteen"
  19 "nineteen"

  20 "twenty"
  30 "thirty"
  40 "forty"
  50 "fifty"
  60 "sixty"
  70 "seventy"
  80 "eighty"
  90 "ninety"

  :decades-separator "-"
  :hundred-separator " hundred"
  :thousand-separator " thousand"
})

(defn transform
  [n]
  (if (get numbers n)
    (get numbers n)
    (cond
      (< n 100)
        (str (transform (* (int (/ n 10)) 10))
             (get numbers :decades-separator)
             (transform (mod n 10)))
      (< n 1000)
        (if (= (mod n 100) 0)
          (str (transform (int (/ n 100)))
               (get numbers :hundred-separator))
          (str (transform (int (/ n 100)))
               (get numbers :hundred-separator)
               " and "
               (transform (mod n 100))))
      (< 1000000)
        (if (= (mod n 1000) 0)
          (str (transform (int (/ n 1000)))
               (get numbers :thousand-separator))
          (str (transform (int (/ n 1000)))
               (get numbers :thousand-separator)
               " and "
               (transform (mod n 1000)))))))



