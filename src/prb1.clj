(ns prb1
  ( :require [clojure.string :as s]))

(def data (->> (slurp "resources/input1")
               (s/split-lines)
               (map #(Integer/parseInt %))
               (sort)))  ;; this makes it a little too easy had to comment it out because first answer was correct.

(defn find-pair
  "Given a list of numbers and a target sum, find a pair that sum to target"
  [n-list target-n]
  (let  [subtracted-data (set (map #(- target-n %) n-list))]
    (filter subtracted-data n-list)))  ;; sets work as a predicate - so find the complements that exist in the original data.

(comment (find-pair data 2020))
; (527 1493)

(def answer-1 (reduce  * (find-pair data 2020))) ;786811

(defn find-triple
  "given a list of number and a target sum, find 3 that sum to target"
  [n-list target-n]
  (let [first-n (first n-list)
        double-target (- target-n first-n)  ;
        pairs (find-pair (rest n-list) double-target)]
    (if (= ( count pairs) 2)  ;if we find a pair that sum to our target - our first number
      (conj pairs first-n) ;;return the first number and pairs
      (recur (rest n-list) target-n))))    ;; else give up on the first number and try again with the rest of the list.

; (1111 289 620)

(def answer-2 (reduce * (find-triple data 2020))) ;;199068980






