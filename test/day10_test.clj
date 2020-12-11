(ns day10-test
  (:require [clojure.test :refer :all])
  (:require [day10 :refer :all]))

(def testdata [28
               33
               18
               42
               31
               14
               46
               20
               48
               47
               24
               23
               49
               45
               19
               38
               39
               11
               1
               32
               25
               35
               8
               17
               7
               9
               4
               2
               34
               10
               3])

(deftest jumps-test
  (is (= 220 (find-answer-1 testdata)))
  (is (= 1885 (find-answer-1 data))))
(deftest jumps2-test
  (is (= 19208 (find-answer2 testdata))))

