(ns day13
  (:require [util :refer [import-lines]]))

(def data (-> (import-lines "resources/input13")))

(def arrival-ts (Integer/parseInt (first data)))

(def schedule
  (-> (rest data)
      (first)
      (clojure.string/split #",")))

(def depart-times (->> (remove #{ "x"} schedule)
                       (map #(Integer/parseInt %))
                       (sort)))

(def answer-1 (->> depart-times
                   (map #(mod arrival-ts %))
                   (zipmap depart-times)
                   (sort-by #(val %))
                   (map #(reduce - %))
                   (zipmap depart-times)
                   (sort-by #(val %))
                   (first)
                   (reduce *)))

(def integerize-val #(for [[k v] %]
                       [k (Integer/parseInt v)]))
(def bus-minutes (->> (zipmap (range ) schedule)
                      (sort-by #(key %))
                      (remove #(#{"x"} (val %)))
                      integerize-val))


(defn bus-iterations [[bus freq]]
  (->> (iterate (partial + freq) 0)
       (filter #(= 0 (mod  % bus)))
       (rest)
       (first)))

(defn find-iterations [acc-mod bus-list]
  (if (empty? bus-list)
    acc-mod
    (do (println (str acc-mod " " bus-list))
      (recur (bus-iterations acc-mod (first bus-list)) (rest bus-list)))))

(->> bus-minutes
     rest
     (sort-by second)
     (map bus-iterations))
(defn find-mods [ln]
  (let [biggest (apply max ln)
        mods (map #(mod biggest % ) ln)]
    mods))







(def lb 100000000000000)
;;1131189818632523 too high
;;3076659627000923