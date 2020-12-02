(ns day1-test
  (:require [clojure.test :refer :all])
  (:require [day1 :refer :all]))

(deftest solutions
  (testing "sol-2"
    (is ( =  199068980 (reduce * (find-triple data 2020)))))
  (testing "sol-1"
   (is (= 786811 (reduce  * (find-pair data 2020))))))

(deftest examples
  (let [data [1721
              979
              366
              299
              675
              1456]]
    (testing "example1"
      (is (= 514579 (reduce * (find-pair data 2020)))))
    (testing "example2"
      (is (= 241861950 (reduce  * (find-triple data 2020)))))))


