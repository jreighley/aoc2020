(ns day7
  (:require [util :refer [import-lines]]))

(def data (->> (import-lines  "resources/input7")))
(defn parse-child [parent child]
  (let [qty (re-find #"[\d]*" child)]
    {:parent parent
     :qty (re-find #"[\d]*" child)
     :child (-> child
                (clojure.string/replace  #" bags" "")
                (clojure.string/replace  (str qty " ") ""))}))
(defn parse-line [line]
  (let [parent-bag (-> (re-find #"[\w|\s]* bags contain" line)
                       (clojure.string/replace " bags contain" "")
                       (clojure.string/trim))
        child-bags (re-seq #"\d[\w|\s]* bag[s]?" line)]
    parse-child (map #(parse-child parent-bag %) child-bags)))

(def all-bags (flatten (map parse-line data)))

(defn find-parent [child]
   (map :parent (filter #(= child (:child %)) all-bags)))
(def ancestors (atom []))
(def sc ["shiny gold"])

(defn find-all-parents [colors]
   (let [search-color (first colors)
         parent-colors (find-parent search-color)
         new-colors (-> colors
                        rest
                        (into parent-colors))]
     (swap! ancestors conj search-color)
     (if (= [] new-colors)
         (dec (count (set @ancestors)))
         (recur new-colors))))

;; not 27, 26









