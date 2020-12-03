(ns day3
  (:require [util :refer [import-lines]]))

(def data (import-lines "resources/input3"))
(def start-postion [ 0 0])
(defn get-char [r c]
  (when (< r (count data))
    (-> (nth data r)
        seq
        cycle
        ( nth c))))

(defn slide [data acc position run rise]
  (let [[cur-col cur-row ] position
        new-col (+ cur-col run)
        new-row (+ cur-row rise)
        new-char (get-char new-row new-col)]
    (if  (nil? new-char)
      (->> acc
           (filter #(=  \# %))
           count)
      (recur data (conj acc new-char) [new-col new-row] run rise))))

(def answer-1 (slide data [\.] [0 0] 3 1)) ;214

(defn slide-slope [run rise]
  (slide data [\.] [0 0] run rise))

(def answer-1 (slide-slope 3 1)) ;314

(def answer-2 (* (slide-slope 1 1)
                (slide-slope 3 1)
                (slide-slope 5 1)
                (slide-slope 7 1)
                (slide-slope 1 2))) ; 8336352024



