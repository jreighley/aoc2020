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



(defn seat-change [seat-state freq-map]
  (cond (= seat-state \L) (if (nil? (get freq-map \#))
                            \#
                            seat-state)
        (= seat-state \#) (if (>= (get freq-map \#) 4)
                            \L
                            \#)
        :else seat-state))



(def state (atom []))

(def leave? [fm])

(def crowded? #(>=   (or (get-in % [:adjacent \#]) -1) 4))
(def vacant? #(nil? (get-in % [:adjacent \L])))

(defn add-next-state [m]
  (assoc m :next-state (cond
                         (= \. (:status m)) \.
                         (vacant? m)  \#
                         (crowded? m)  \L
                         :else (:status m))))

(defn add-adjacent-info [m]
  (let [row (:row m)
        col (:col  m)
        row-set #{(inc row) row (dec row)}
        col-set #{(inc col) col (dec col)}]
       (->> @state
         (filter #(row-set (:row %)))
         (filter #(col-set (:col %)))
         (remove #(and(= row (:row %))
                      (= col (:col %))))
         (map :status)
         (frequencies)
         (assoc m :adjacent))))


;; intial setup

(defn parse-seat-status [input [r c]]
  (let [rowstr (nth input r)
        colstr (nth rowstr c)]
    colstr))

(defn map-seats [input]
  (let [seat-statuses (for [[r c]  (memo-cell-list input)]
                       {:row r :col c :status  (parse-seat-status input [r c])})]
   (reset! state seat-statuses)))

(def crowded? #(>=   (or (get-in % [:adjacent \#]) -1) 4))
(def vacant? #(nil? (get-in % [:adjacent \#])))







