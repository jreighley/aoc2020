(ns day14
  (:require [util :refer [import-lines]]))

(def data (-> (import-lines "resources/input14")))
(Long/toBinaryString 950)
(defn pad36 [s]
  (apply str (conj (vec (repeat (- 36 (count s))"0"))s)))

(defn binaryize [[mem val]]
  [mem (pad36 (Long/toBinaryString (Long/parseLong val)))])

(def find-mask #(re-seq #"[\d|X]{36}" %))
(def memory-loc #(re-seq #"\d*" %))
(def memory-input? #(re-seq #"mem\[" %))

(defn parse-location-value [line]
  (->> (memory-loc line)
       (remove empty?)
       (binaryize)))

(defn val-to-write [mask val]
  (apply str
         (for [bit (range 36)]
           (if (= \X (nth mask bit))
               (nth val bit)
               (nth mask bit)))))

(defn update-mem [mem mask [loc val]]
       (assoc mem loc (val-to-write mask val)))
(def bin->long #(Long/valueOf % 2))

(defn process-prog [mem mask prog]
  (let [process-line (first prog)
        next-mask (if (find-mask process-line)
                    (first (find-mask process-line))
                    mask)
        mem-update (when (memory-input? process-line)
                     (parse-location-value process-line))
        new-mem (if mem-update
                  (update-mem mem mask mem-update)
                  mem)]
    (if (empty? (rest prog))
      (reduce + (map bin->long (vals new-mem)))
      (recur new-mem next-mask (rest prog)))))


;; 12081666704507 too low.
