(ns util
  (:require [clojure.string :as s]))

(defn import-lines [file]
  (->> (slurp file)
       (s/split-lines)))