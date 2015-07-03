(ns euler.numbers-test
  (:require [clojure.test :refer :all]
            [euler.numbers :refer :all]))

(deftest naturals-test
  (testing "Getting natural numbers"
    (is (= [1] (take 1 naturals)))))
