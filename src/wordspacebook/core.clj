(ns wordspacebook.core
  (:require [clojure.string :as str])
  (:use [incanter.stats :only (levenshtein-distance)]))

(defn leven
  "Return edit distance between 2 strings.
   From: http://www.learningclojure.com/2010/11/levenshtein-distance-edit-distance.html"
  [s1 s2]
  (cond
   (empty? s1) (count s2)
   (empty? s2) (count s1)
   :else (min (+ (if (= (first s1) (first s2)) 0 1)
                 (leven (rest s1) (rest s2)))
              (inc (leven (rest s1) s2))
              (inc (leven s1 (rest s2))))))

(defn friend?
  "Return edit distance between 2 strings.
   From: http://www.learningclojure.com/2010/11/levenshtein-distance-edit-distance.html"
  [s1 s2]
  (cond
   (empty? s1) (count s2)
   (empty? s2) (count s1)
   :else (min (+ (if (= (first s1) (first s2)) 0 1)
                 (leven (rest s1) (rest s2)))
              (inc (leven (rest s1) s2))
              (inc (leven s1 (rest s2))))))

(leven "foo" "bar")

(def x (slurp "word.list"))

(def y (str/split x #"\n"))

(def word-list (set y))

(defn find-buddies-v1 [s coll]
  (loop [rem coll
         matches []]
    (cond
     (empty? rem) matches
     (= 1 (leven s (first rem))) (recur (rest rem) (conj matches (first rem)))
     :else (recur (rest rem) matches))))

(defn filter-length [l coll]
  (filter #(= l (count %)) coll))

(defn make-len-to-words [coll]
  (->> (range 1 32)
       (map #(vector % (filter-length % coll)))
       (reduce #(assoc %1 (first %2) (second %2)) {})))

(defn find-buddies [s coll len-to-words]
  (let [len (count s)
        above (get len-to-words (inc len) [])
        below (get len-to-words (dec len) [])
        coll (filter #(= len (count %)) coll)]
    (loop [rem coll
           matches []]
      (cond
       (empty? rem) matches
       (= 1 (leven s (first rem))) (recur (rest rem) (conj matches (first rem)))
       :else (recur (rest rem) matches)))))

(time (find-buddies "abcdefgh" word-list (make-len-to-words word-list)))


#_(time (nil? (doall (map #(find-buddies % word-list) (take 2 word-list)))))

#_(time (nil? (find-buddies "cauldillos" word-list)))

(defn off-by-one? [s1 s2]
  (<= (Math/abs (- (count s1) (count s2))) 1))

(defn substr-hash-match? [s1 s2]
  (let [len1 ]))

(defn is-buddy? [s1 s2]
  (cond
   (> 1 (Math/abs (- (count s1) (count s2)))) false
   (off-by-one? s1 s2) (substr-hash-match? s1 s2)
   :else (one-char-delta? s1 s2)))



(defn find-length-possibles [s coll]
  (filter #(< (Math/abs (- (count s) (count %))) 1) coll))

(doseq [w (->> (range 1 40)
               (map #(repeat % "a"))
               (map #(apply str %)))]
  (println (count w) (count (find-length-possibles w word-list))))

(time (count (find-length-possibles "abc" word-list)))
















