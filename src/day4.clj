(ns day4
  (:require [util :refer [import-lines]]))

(def data (import-lines "resources/input4"))

(defn parse-line [line]
  (when (not (empty? line)) (let [datachunks (clojure.string/split line #" ")
                                  pairs (map  #(clojure.string/split % #":") datachunks)]
                              (reduce conj (for [ [k v] pairs]
                                             {(keyword k) v})))))

(def fourdigits? #(= ( count %) 4))

(def enough-pairs? #(->> %
                         keys
                         (keep #{ :hgt :iyr :eyr :pid :byr :hcl :ecl})
                         (count)
                         #{7}))

(defn valid-hgt? [m]
  (let [hgt (:hgt m)
        units (apply str  (take-last 2 hgt))
        measure (apply str (take (-  (count hgt) 2) hgt))]
    (and (#{"cm" "in"} units)
         (if (= units "cm")
           (<= 150 (Integer/parseInt measure) 193)
           (<= 59 (Integer/parseInt measure) 76)))))

(defn valid-byr? [ {byr :byr}]
      (and (fourdigits? byr)
           (<= 1920 (Integer/parseInt byr) 2002)))

(defn valid-iyr? [ { iyr :iyr}]
  (and (fourdigits? iyr)
       (<= 2010 (Integer/parseInt iyr) 2020)))

(defn valid-eyr? [ { eyr :eyr}]
    (and (fourdigits? eyr)
         (<= 2020 (Integer/parseInt eyr) 2030)))

(defn valid-ecl? [ {ecl :ecl}]
     (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl))

(defn valid-pid? [{pid :pid}]=
    (and (#{9} (count pid))
         (re-find #"\d{9}" pid)))

(defn valid-hcl? [m]
  (let [hcl (:hcl m)
        hash-start? (= \# (first hcl))]
      (and hash-start?
        (= 6 (count (keep #{\a \b \c \d \e \f \0 \1 \2 \3 \4 \5 \6 \7 \8 \9}  (rest  hcl)))))))

(def pp-list (->> data
                  (map parse-line)
                  (partition-by #( nil? %))
                  (map #(reduce conj %))
                  (filter some?)))

(def answer-1 (->> pp-list
                   (filter enough-pairs?)
                   count)) ;202 correct answer 1

( def answer-2 (->> pp-list
                    (filter enough-pairs?)
                    (filter valid-byr?)
                    (filter valid-iyr?)
                    (filter valid-eyr?)
                    (filter valid-hgt?)
                    (filter valid-hcl?)
                    (filter valid-ecl?)
                    (filter valid-pid?)
                    (count))) ;; 137 is correct

