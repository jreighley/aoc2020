(ns day5
  (:require [util :refer [import-lines]]))

(def data (import-lines "resources/input5"))
(def rows (range 1 129))
(def cols (range 1 9))

(def dirfn {\F take
            \B take-last
            \L take
            \R take-last})

(defn find-row [dirfns inv]
  (let [dir (dirfn (first dirfns))
        amount (/ (count inv) 2)
        newrows (dir amount inv)]
    (if (zero? (count (rest dirfns)))
      (dec (first newrows))
      (recur (rest dirfns ) newrows))))

(defn dirs [dirstr]
  (+ (* 8 (find-row (take 7 dirstr) rows))
     (find-row (take-last 3 dirstr) cols)))

(def answer-1 (reduce max (map dirs data))); 901 correct

(def answer-2 (let [possible-seats (remove (set (map dirs data))
                                           (for [x (map dec rows)
                                                 y (map dec cols)]
                                            (+ (* 8 x)
                                               y)))
                    ticket-list  (map dirs data)
                    left-tickets (set (map inc ticket-list))
                    right-tickets (set (map dec ticket-list))]
                 (first (->> possible-seats
                          (filter left-tickets)
                          (filter right-tickets))))) ;661 correct








