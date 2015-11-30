(ns euler.british_numbers_to_chars_test
  (:require [clojure.test :refer :all]
            [euler.british_numbers_to_chars :refer :all]))

(deftest tranform-test
  (testing "Getting numbers below 20 correctly"
    (is (= "zero" (transform 0)))
    (is (= "one" (transform 1)))
    (is (= "two" (transform 2)))
    (is (= "three" (transform 3)))
    (is (= "four" (transform 4)))
    (is (= "five" (transform 5)))
    (is (= "six" (transform 6)))
    (is (= "seven" (transform 7)))
    (is (= "eight" (transform 8)))
    (is (= "nine" (transform 9)))
    (is (= "ten"  (transform 10)))
    (is (= "eleven"  (transform 11)))
    (is (= "twelve"  (transform 12)))
    (is (= "thirteen"  (transform 13)))
    (is (= "fourteen"  (transform 14)))
    (is (= "fifteen"  (transform 15)))
    (is (= "sixteen"  (transform 16)))
    (is (= "seventeen"  (transform 17)))
    (is (= "eighteen"  (transform 18)))
    (is (= "nineteen"  (transform 19))))

  (testing "Getting numbers below 100 correctly"
    (is (= "twenty" (transform 20)))
    (is (= "thirty" (transform 30)))
    (is (= "forty" (transform 40)))
    (is (= "fifty" (transform 50)))
    (is (= "sixty" (transform 60)))
    (is (= "seventy" (transform 70)))
    (is (= "eighty" (transform 80)))
    (is (= "ninety" (transform 90)))

    (is (= "twenty-one" (transform 21)))
    (is (= "thirty-two" (transform 32)))
    (is (= "forty-four" (transform 44)))
    (is (= "fifty-five" (transform 55)))
    (is (= "sixty-six" (transform 66)))
    (is (= "seventy-seven" (transform 77)))
    (is (= "eighty-eight" (transform 88)))
    (is (= "ninety-nine" (transform 99))))

  (testing "Getting numbers below 999 correctly"
    (is (= "five hundred and twenty-five" (transform 525)))
    (is (= "nine hundred and ninety-nine" (transform 999)))
    (is (= "two hundred" (transform 200))))

  (testing "Getting numbers above 999 correctly"
    (is (= "one thousand" (transform 1000)))))
