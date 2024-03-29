(ns wordspacebook.core
  (:require [clojure.string :as str])
  (:use [incanter.stats :only (levenshtein-distance)]))

(set! *warn-on-reflection* true)

(defn filter-length [l coll]
  (filter #(= l (count %)) coll))

(defn make-word-map
  "Pre-process word list to return a map of char-length -> [words]"
  [coll]
  (->> (range 1 32)
       (map #(vector % (set (filter-length % coll))))
       (reduce #(assoc %1 (first %2) (second %2)) {})))

(defn within-distance [s coll]
  (filter #(< (Math/abs (- (count s) (count %))) 2) coll))

(defn within-distance-pre [s word-map]
  (let [len (count s)]
    (concat (get word-map (dec len))
            (get word-map len)
            (get word-map (inc len)))))

(defn off-by-one? [s1 s2]
  (= (Math/abs (- (count s1) (count s2))) 1))

(defn substr? [^String longer ^String shorter]
  (.contains longer shorter))

(defn substr-hash-match? [s1 s2]
  (let [len1 (count s1)
        len2 (count s2)
        shorter (if (< len1 len2) s1 s2)
        longer (if (< len1 len2) s2 s1)]
    (substr? longer shorter)))


(defn one-char-delta? [num-diff s1 s2]
  (loop [num-diff num-diff
         s1 s1
         s2 s2]
    (cond
     (> num-diff 1) false
     (empty? s1) (<= (+ num-diff (count s2)) 1)
     (empty? s2) (<= (+ num-diff (count s1)) 1)
     (= (first s1) (first s2)) (recur num-diff (rest s1) (rest s2))
     :else (recur (inc num-diff) (rest s1) (rest s2)))))

(defn friend? [s1 s2]
  (cond
   (off-by-one? s1 s2) (substr-hash-match? s1 s2)
   (one-char-delta? 0 s1 s2) true
   :else false))

(defn find-friends [s word-map]
  (filter #(friend? s %) (within-distance-pre s word-map)))

(defn assoc-friends [m s word-map]
  (let [friends (find-friends s word-map)
        m (assoc m s friends)]
    (reduce #(assoc %1 %2 (conj (get %1 %2) s)) m friends)))

(defn remove-from-word-map [word word-map]
  (assoc word-map
    (count word) (disj (get word-map (count word)) word )))

(defn make-graph [word-list]
  (loop [graph {}
         word-list word-list
         word-map (make-word-map word-list)]
    (if (empty? word-list)
      graph
      (recur (assoc-friends graph (first word-list) word-map)
             (rest word-list)
             (remove-from-word-map (first word-list) word-map)))))

(defn find-length-possibles [s coll]
  (filter #(< (Math/abs (- (count s) (count %))) 1) coll))

;;;

(def x (slurp "word.list"))

(def y (str/split x #"\n"))

(def word-list-sorted y)

(def word-list (set y))

(def word-vec-list (set (map vec y)))

(def word-map (make-word-map word-list-sorted))

(def word-vec-map (make-word-map word-vec-list))

#_(get word-map 3)

#_(time (nil? (make-graph (set (take 3000 word-list)))))

#_(time (nil? (doall (map #(find-buddies % word-list) (take 2 word-list)))))

#_(time (nil? (find-buddies "cauldillos" word-list)))

#_(take 10 word-list-sorted)

#_(bit-and (.getBytes "foo") (.getBytes "bar"))

#_(map bit-or (.getBytes "foo") (.getBytes "foo") )

;;;;

(defn network-for [word word-map levels max-levels]
  (let [word-map (remove-from-word-map word word-map)
        friends (find-friends word word-map)]
    (cond
     (>= levels max-levels) {word friends}
     :else (->> friends
                (pmap #(network-for % (remove-from-word-map % word-map) (inc levels) max-levels))
                (reduce merge {word friends})))))

(def output-dir "./graph")

(defn word-path [word]
  (str output-dir "/" word))

(defn write-word-file [word word-map]
  (spit (word-path word) (pr-str (find-friends word word-map))))

#_(write-word-file (first word-list-sorted) word-map)

(let [total (count word-list-sorted)]
  (println "Running complete make graph :" total)
  (time (doall (pmap #(write-word-file % word-map) word-list-sorted)))
  (println "Done."))


(defn count-network [network]
  (->> network
       (map #(count (second %)))
       (reduce +)))

(use 'clojure.pprint)
#_(time (pprint (count-network (network-for "causes" word-map 0 3))))
#_(time (pprint (count-network (network-for (vec "causes") word-vec-map 0 1))))


