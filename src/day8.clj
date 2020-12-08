(ns day8
  (:require [util :refer [import-lines]]))

(def data (->> (import-lines  "resources/input8")))

(def cmd-fn
  {"jmp" #(update % :pointer  +  (:value %))
   "nop" #(update % :pointer inc)
   "acc" #(-> %
            (update :acc + (:value %))
            (update :pointer inc))})

(defn make-int [s]
  (Integer/parseInt s))
(defn find-int [op-line]
  (make-int (subs op-line 4)))

(declare pointerlist)
(defn process [ pointer acc]
  (let [operation (nth data pointer)
        command (apply str (take 3 operation))
        value  (find-int operation)
        op-fn (get cmd-fn command) ;command)
        new-map (op-fn   {:cmd command :value value :pointer pointer :acc acc})]

    (if (or (@pointerlist pointer) (= pointer (dec (count data))))
      new-map
      (do (swap! pointerlist conj pointer)
          (recur  (:pointer new-map) (:acc new-map))))))


(defn find-loop []
  (def pointerlist (atom #{}))
  (process 0 0)) ;; {:pointer 52, :acc 1200, :value -391}

(def pointerlist2 (atom #{}))
(def pointerpossiblities (atom @pointerlist))
(def swap-func {"jmp" "nop"
                "nop" "jmp"
                "acc" "acc"})

(def answer-2 (atom []))

(defn process2 [ pointer acc swapable]
  (if (= pointer 643)
      (swap! answer-2 conj {:line swapable :acc acc})
      (let [operation (nth data pointer)
            command (apply str (take 3 operation))
            swap-fn (if (= pointer swapable)
                      (get swap-func command)
                      command)
            value  (find-int operation)
            op-fn (get cmd-fn swap-fn)
            new-map (op-fn   {:cmd command :value value :pointer pointer :acc acc :swapable swapable})]
        (if (or (@pointerlist2 pointer) (= pointer (count data)))
          new-map
          (do (swap! pointerlist2 conj pointer)
              (recur  (:pointer new-map) (:acc new-map) swapable))))))
(defn swap-run [swapable]
  (reset! pointerlist2 #{})
  (reset! pointerlist #{})
  (process2 0 0 swapable))

(defn find-answer-2 []
  (reset! answer-2 {})
  (reset! pointerlist #{})
  (find-loop)
  (dorun (map #(swap-run %) (range 0 642)))
  @answer-2)  ;;["winning swap 327" " acc" 1023]


