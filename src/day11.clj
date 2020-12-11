(ns day11
  (:require [util :refer [import-lines]]))

(def data
  (->> (slurp "resources/input11")
       (clojure.string/split-lines)
       vec))

(defn cell-list [input]
  (for [r (range (count input))
        c (range (count (first input)))]
    [r c]))

(def memo-cell-list (memoize cell-list))


(defn find-neighbors [data [r c]]
  (let [allowed-cells? (set (memo-cell-list data))
        neighbor-rows  #{(inc r) r (dec r)}
        neighbor-cols  #{(inc c) c (dec c)}]
   {:rows neighbor-rows
    :cols neighbor-cols}
   (filter allowed-cells? (for [nr neighbor-rows
                                nc neighbor-cols
                                :when (not (and (= nr r)
                                                (= nc c)))]
                               [nr nc]))))

(defn parse-seat-status [input [r c]]
  (let [rowstr (nth input r)
        colstr (nth rowstr c)]
    colstr))

(defn seat-change [seat-state freq-map]
  (cond (= seat-state \L) (if (nil? (get freq-map \#))
                            \#
                            seat-state)
        (= seat-state \#) (if (>= (get freq-map \#) 4)
                            \L
                            \#)
        :else seat-state))

(defn new-seat-state [input [r c]]
  (let [freq-map (->> (find-neighbors input [r c])
                      (map #(find-seat-status input %))
                      (frequencies))
        seat-state (find-seat-status input [r c])]
    (seat-change seat-state freq-map)))

(defn recursive-change [input]
  (let [new-state   (map #(apply str %) (partition 98 (for [[row col] (memo-cell-list input)]
                                                        (new-seat-state input [row col]))))]
    #_(if (= input new-state)
        input
        (recur new-state))
    new-state))

(defn add-neigbor-status [input [{r :row c :col }m]]
  (let [neighbor-rows  #{(inc r) r (dec r)}
        neighbor-cols  #{(inc c) c (dec c)}]
   (for [nr neighbor-rows
         nc neighbor-cols
         :where (not (and (= nr r)
                          (= nc c)))]
     (filter ))))


(defn map-seats [input]
  (for [[r c]  (memo-cell-list input)]
   {:row r :col c :status  (parse-seat-status input [r c])}))



