(ns day9
  (:require [util :refer [import-lines]]))

(defn make-int [s]
  (Long/parseLong s))

(def data (->> (import-lines  "resources/input9")
            (map make-int)))

(defn parse26 [list]
  (let [target-list (set (take 25 list))
        next-n (nth list 25)
        complements (set (map #(- next-n %) target-list))
        intersecting (clojure.set/intersection complements target-list)
        good? (not (empty? intersecting))]
     (if (not good?)
       next-n
       (recur (rest list)))))

(defn find-answer-1 [] (parse26 data)) ;;542529149 correct

(defn find-542529149 [list]
  (if ((set (reductions + list)) 542529149)
    list
    (recur (rest list))))

(defn find-answer-2 []
  (let [encoded-seq (find-542529149  data)
        seq-leg (dec (count (take-while pos? (reductions - 542529149 encoded-seq))))
        decoded-seq (doall (take seq-leg encoded-seq))]
   (+ (reduce min decoded-seq)
      (reduce max decoded-seq)))) ;75678618

