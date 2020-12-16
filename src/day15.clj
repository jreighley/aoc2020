(ns day15)

(def start-seq [9,19,1,6,0,5,4])

(defn next-n [sq]
  (let [n-ns (count sq)
        last-said (.lastIndexOf (butlast sq)(peek sq))
        next-number (if (= -1  last-said)
                      0
                      (- n-ns
                         (inc last-said)))]
    (conj sq next-number)))

;;counting in a recursive function like this works okay for 2020 items.
;; Probably less ideal on 30million..

(defn get2020 [sq]
  (if (= (count sq) 2020)
    (last sq)
    (recur (next-n sq))))

(defn get3 [sq]
  (if (= (count sq) 30000000)
    (last sq)
    (recur (next-n sq))))

(get2020 start-seq)  ;1552 is correct
(comment (get3 [0,3,6])
         (get3 start-seq))
;; not 182 or 57 or 8

(defn last-seen [history n]
  (get history n))
;; try do do it faster...

(defn better-next-n [history]
  (let [{lastn :last-n turn :turn limit :limit} history
        seen (last-seen history lastn)
        nextn (if (nil? seen)
                0
                (-  turn seen))
        new-history (conj history {:last-n nextn :turn (inc turn) lastn turn})]
    (if (= turn limit)
      lastn
      (recur new-history))))

(defn make-history [sq limit]
  (conj {:turn (count sq)
         :last-n (last sq)
         :limit limit}
        (zipmap  (butlast sq)  (map inc(range)))))

(defn find-nth-said [n]
  (->> (make-history start-seq n)
       (better-next-n)))  ;;18234 was correct

(comment (time (find-nth-said 30000000)))  ;;33 seconds or so..





