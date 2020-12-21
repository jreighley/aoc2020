(ns day20
  (:require [util :refer [import-lines]]))

(def data (-> (import-lines "resources/input20")))

(def unparsed-tiles (->> (partition-by #(= "" %) data)
                         (remove #(= '("") %))))

(defn parse-tile [tile]
  (let [id (subs (first tile) 5 9)
        n (nth tile 1)
        s (last tile)
        e (apply str (map first (rest tile)))
        w (apply str (map last (rest tile)))]
    {:id id :n n :s s :e e :w w
     :edges #{n s e w}}))

(def tiles (map parse-tile unparsed-tiles))
(defn add-possible-partners [tileset]
  (remove nil? (for [p-tile tileset
                     tile tileset
                     :when (not (= (:id tile) (:id p-tile)))]
                 (if-let [neighbor-id (not-empty (clojure.set/intersection (:edges p-tile) (:edges tile)))]
                   {:tile (:id tile)
                    :neighbors {:boundry neighbor-id
                                :with (:id p-tile)}}))))

(def tile-boundries (add-possible-partners tiles))
(group-by val (frequencies (map #(:tile  %) tile-boundries)))