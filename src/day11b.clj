(ns day11b
  (:require [util :refer [import-lines]]))

(def data
  (->> (slurp "resources/input11")
       (clojure.string/split-lines)
       vec))

(def testdata   (->> (slurp "resources/day11test")
                     (clojure.string/split-lines)
                     vec))

(defn adj-n [n]
  [(dec n) n (inc n)])

(defn adj-cells [[r c]]
  (into #{} (for [row (adj-n r)
                  col (adj-n c)
                  :when (not (and  (= row r)
                                   (= col c)))]
              [row col])))

(def state (atom []))

(defn base-read [dataset]
  (into [] (for [row (range (count dataset))
                 col (range (count (first dataset)))]
             {:lookup [row col]
              :sts (-> dataset
                     (nth row)
                     (nth col))
              :adj (adj-cells [row col])})))


(defn find-cell [ lookup]
  (filter #(= (:lookup %) lookup) @state))

(defn adjacent-crowd [lookup]
  (let [cell (-> lookup
                 find-cell
                 first)
        adjacents (:adj cell)
        adj-occs (->> @state
                      (filter #(adjacents (:lookup %)))
                      (filter #(= \# (:sts %))))
        crowd? (when adj-occs
                 (if (<= 4 (count adj-occs)) true false))
        vac? (if (empty? adj-occs) true false)]
     (cond
       crowd? \L
       vac? \#)))

(defn next-cell-state [lookup]
  (let [old-cell-state (first (find-cell lookup))
        new-cell-state (if (=  (:sts old-cell-state))
                         (:sts old-cell-state)
                         (adjacent-crowd lookup))]
    (assoc old-cell-state :sts new-cell-state)))

(defn build-process [dataset]
  (->> (base-read dataset)
       (reset! state)))

(defn process []
  (let [newstate (sort-by :lookup (into [](for [cell @state]
                                             (next-cell-state (:lookup cell)))))]
    (if (= newstate @state)
      @state
      (do (reset! state newstate)
          (recur)))))





