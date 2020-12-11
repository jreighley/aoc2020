(ns day10
  (:require [util :refer [import-lines]]))

(defn make-int [s]
  (Long/parseLong s))

(def data (->> (import-lines  "resources/input10")
               (map make-int)
               (vec)))

(defn  joltage-rating [list]  (+ 3 (reduce max list)))

(defn joltage-jumps  [list ] (sort (conj list 0 (joltage-rating list))))

(defn jumps [jump-list list]
  (let [new-jump (- (second list) (first list))
        new-jump-list (conj jump-list new-jump)]
   (if (= 2 (count list))
     (reduce * (vals (frequencies new-jump-list)))
     (recur new-jump-list (rest list)))))

(defn find-answer-1 [list]
  (jumps [] (joltage-jumps list))) ;; 1885 correct 0.607586 msecs


(defn jumps2 [jump-list list]
  (let [new-jump (- (second list) (first list))
        new-jump-list (conj jump-list new-jump)]
    (if (= 2 (count list))
      new-jump-list
      (recur new-jump-list (rest list)))))

(def possibles {'(1 1 1 1) 4
                '(1 1 1)   3
                '(1 1)     2
                '(1)       1
                '(3)       1
                '(3 3)     1})

(defn find-answer2 [list]
  (->> (jumps2 [] (joltage-jumps list))
   ))



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


;177147 low
;2834352 low
;22674816 low
;67108864
;1811939328