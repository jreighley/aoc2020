(ns day12
  (:require [util :refer [import-lines]]))

(def data (import-lines "resources/input12"))

(def command-list (for [cmd data]
                    [(subs cmd 0 1) (Integer/parseInt (subs cmd 1))]))

(def axis
  {"N" :y-loc "S" :y-loc "E" :x-loc "W"  :x-loc "R" :z-b "L" :z-b})
(def dir
  {"N" + "S" - "E" + "W"  - "R" + "L" -})
(def cb
  {90 "E" 180 "S" 270 "W" 0 "N"})

(defn convert-f [state  mag]
  [(get cb (mod (:z-b state) 360)) mag])

(def ship-state {:x-loc 0
                 :y-loc 0
                 :z-b 90})

(defn process-cmd [[cmd mag] state]
  (let [cmd-axis (get axis cmd)
        cmd-dir (get dir cmd)
        f-cmd (convert-f state  mag)]
    (println cmd)
    (if (= "F" cmd)
      (process-cmd  f-cmd state)
      (update state cmd-axis cmd-dir mag))))

(defn process-all [list state]
  (if (empty? list)
    (+ (Math/abs (:x-loc state)) (Math/abs (:y-loc state)))
    (recur (rest list) (process-cmd (first list) state))))

(def answer-1 (process-all command-list ship-state))  ;415 is correct

(def state2 {{:x-loc 0
              :y-loc 0
              :z-b 0
              :wp-y 10
              :wp-y 1}})

(def wp-axis
  {"N" :wp-y "S" :wp-y "E" :wp-y "W"  :wp-y "R" :z-b "L" :z-b})

(defn move-waypoint [[cmd mag]])





