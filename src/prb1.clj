(ns prb1
  ( :require [clojure.string :as s]))

(def data (->> (slurp "resources/input1")
               (s/split-lines)
               (map #(Integer/parseInt %))
               (sort)))

(defn find-pair [n-list target-n]
  (let  [subtracted-data (set (map #(- target-n %) n-list))]
    (filter subtracted-data n-list)))  ;786811

(def answer-1 (reduce  * (find-pair data 2020))) ;786811

(defn find-triple [n-list target-n]
  (let [first-n (first n-list)
        double-target (- target-n first-n)
        pairs (find-pair (rest n-list) double-target)]
    pairs))  ;  would need recursion if the first number wasn't a winner..
;;(* 620 1111 289) 199068980






