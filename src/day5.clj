(ns day5
  (:require [util :refer [import-lines]]))

(def data (import-lines "resources/input5"))
(def rows (range 1 129))
(def cols (range 1 9))

(def dirfn {\F take
            \B take-last
            \L take
            \R take-last})

(defn calc-row-or-col [dirfns rc-inv]
  (let [dir (dirfn (first dirfns))
        amount (/ (count rc-inv) 2)
        newrows (dir amount rc-inv)]
    (if (= 1 amount) ;once we parse 2 in half, there is no more to parse.
      (dec (first newrows))
      (recur (rest dirfns ) newrows))))

(defn dirs [dirstr]
  (+ (* 8 (calc-row-or-col (take 7 dirstr) rows))
     (calc-row-or-col (take-last 3 dirstr) cols)))

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








