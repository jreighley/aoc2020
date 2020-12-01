(ns prb1
  ( :require [clojure.string :as s]))

(def data (->> (slurp "resources/input1")
               (s/split-lines)
               (map #(Integer/parseInt %))
               (sort)))  ;; this makes it a little too easy had to comment it out because first answer was correct.

(defn find-pair [n-list target-n]
  (let  [subtracted-data (set (map #(- target-n %) n-list))]
    (filter subtracted-data n-list)))

(comment (find-pair data 2020))
; (527 1493)

(def answer-1 (reduce  * (find-pair data 2020))) ;786811

(defn find-triple [n-list target-n]
  (let [first-n (first n-list)
        double-target (- target-n first-n)
        pairs (find-pair (rest n-list) double-target)]
    (if (= ( count pairs) 2)
      (conj pairs first-n)
      (recur (rest n-list) target-n))))  ; (1111 289 620)

(def answer-2 (reduce * (find-triple data 2020))) ;;199068980






