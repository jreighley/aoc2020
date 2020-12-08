(ns day6
  (:require [util :refer [import-lines]]
            [clojure.set :as cs]))

(def data (->> (import-lines  "resources/input6")
               (partition-by empty?)))

(defn find-answer-1 []
  (->> data
    (map #(apply str %))
    (remove empty?)
    (map set)
    (map count)
    (reduce +))) ; 6310 correct

(defn unanimous? [group]
  (->> (map set  group)
    (reduce cs/intersection)
    count))

(defn find-answer-2 []
  (->> data
       (remove empty?)
       (map unanimous?)
       (reduce +)))  ;3193 correct

