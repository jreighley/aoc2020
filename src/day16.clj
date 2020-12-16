(ns day16
  (:require [util :refer [import-lines]]))

(def data (-> (import-lines "resources/input16")))
(def valid-range-re #".*:.\d*-\d* or \d*-\d*\s")
(def digit-re #"\d*-\d*")
(def name-re #".*:")
(defn name-kw [s]
  (-> s
    (clojure.string/replace ":" "")
    (clojure.string/replace " " "")
    (keyword)))

(defn range->set [[lb ub]]
  (range lb (inc ub)))

(defn make-set-from-range [str]
  (->> (re-seq #"\d*" str)
       (remove empty?)
       (map #(Integer/parseInt %))
       (range->set)))

(defn process-valid-list [line]
   (let [name (re-find name-re line)
         ranges (re-seq digit-re line)]
     (when (and (string? name)
                (seq? ranges))
       {(name-kw name) (reduce into #{} (map make-set-from-range ranges))})))

(def valid-ranges (reduce conj (map process-valid-list data)))
(reduce into #{} (vals valid-ranges))
(def answer-1  (->> (drop 25 data)
                    (map #(clojure.string/split % #","))
                    (flatten)
                    (map #(Integer/parseInt %))
                    (remove (reduce into #{} (vals valid-ranges)))
                    (reduce +))) ;20048 is correct

(defn make-int-list [ticket]
  (map #(Integer/parseInt %) ticket))

(def tickets (->> (drop 25 data)
                  (map #(clojure.string/split % #","))
                  (map make-int-list)))

(def csv-sets (reduce conj (for [n (range (count (first tickets)))]
                             {n (into #{} (map #(nth % n) tickets))})))

(for [valid-ticket valid-ranges
      ticket# csv-sets]
  {(key ticket#) {(key valid-ticket) (count (remove (val valid-ticket) (val ticket#)))}})


csv-sets





