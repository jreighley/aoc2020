(ns day2
  (:require [util :refer [import-lines]]))

(def data (import-lines "resources/input2"))

(defn parse-line [line]
  (let [[policies letter password]   (clojure.string/split line #" ")
        [minlen max-len] (clojure.string/split policies #"-")
        letter (subs letter 0 1)]
    {:min-count (Integer/parseInt  minlen)
     :max-count (Integer/parseInt  max-len)
     :letter letter
     :password password}))

(defn add-char-count [policy-map]
  (let [ char-count (->> (:password policy-map)
                         (re-seq (re-pattern (:letter policy-map)))
                         (count))]
   (assoc policy-map :char-count char-count)))

(def pw-info (map parse-line data))

(def valid-pw? #(<= (:min-count % ) (:char-count %) (:max-count %)))

(defn find-answer1 [] (->> pw-info
                           (map add-char-count)
                           (filter valid-pw?)
                           (count)))  ;560
(defn find-letter [n s]
  (let [max-n (count s)
        first-char (when (<= n max-n)
                     (subs s (dec n ) n))]
    first-char))

(defn add-key-chars [m]
  (let [ char-a (find-letter (:min-count m) (:password m))
        char-b (find-letter (:max-count m) (:password m))]
    (conj m {:char-a char-a :char-b char-b})))

(def different? #(not (= (:char-a %) (:char-b %))))

(def correct? #(or ( = (:letter %) (:char-b %)) ( = (:letter %) (:char-a %))))

(defn find-answer2 [] (->> pw-info
                           (map add-key-chars)
                           (filter correct?)
                           (filter different?)
                           (count))) ;303

(comment
   (def testdata ["1-3 a: abcde" "1-3 b: cdefg""2-9 c: ccccccccc"])
   (->> testdata
      (map parse-line)
      (map add-key-chars)
      (filter correct?)
      (filter different?)
      (count)))


