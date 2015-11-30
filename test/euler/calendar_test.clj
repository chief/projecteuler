(ns euler.calendar_test
  (:require [clojure.test :refer :all]
            [euler.calendar :refer :all]))

(deftest leap-year-test
  (testing "Inspecting leap years"
    (is (= false (leap-year 1941)))
    (is (= true (leap-year 1940)))
    (is (= false (leap-year 1900)))
    (is (= true (leap-year 2000)))))

(deftest starting-month-days-test
  (testing "Aligning months in year"
    (is (= [0 31 59 90 120 151 181 212 243 273 304 334 365]
           (first (starting-month-days 1901))))
    (is (= [0 31 59 90 120 151 181 212 243 273 304 334 365]
           (first (starting-month-days 1900)))))

  (testing "Having an offset"
    (is (= [0 41 69 100 130 161 191 222 253 283 314 344 375]
           (first (starting-month-days 1901 10))))))

(deftest month-days-period-test
  (testing "Getting month days from period"
    (is (= [0 31 59 90 120 151 181 212 243 273 304 334 365 396 424 455 485 516
            546 577 608 638 669 699 730] (month-days-period 1900 1901)))))