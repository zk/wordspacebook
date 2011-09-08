(ns wordspacebook.core
  (:require [clojure.string :as str])
  (:use [incanter.stats :only (levenshtein-distance)]))

(defn off-by-one? [s1 s2]
  (= (Math/abs (- (count s1) (count s2))) 1))

(defn substr? [longer shorter]
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

(defn is-buddy? [s1 s2]
  (cond
   (> (Math/abs (- (count s1) (count s2))) 1) false
   (off-by-one? s1 s2) (substr-hash-match? s1 s2)
   (one-char-delta? 0 s1 s2) true
   :else false))


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

(defn find-buddies [s coll]
  (loop [rem (filter #(< (Math/abs (- (count s) (count %))) 2) coll)
         matches []]
    (cond
     (empty? rem) matches
     (is-buddy? s (first rem)) (recur (rest rem) (conj matches (first rem)))
     :else (recur (rest rem) matches))))

#_(time (nil? (doall (map #(find-buddies % word-list) (take 2 word-list)))))

#_(time (nil? (find-buddies "cauldillos" word-list)))


(defn find-length-possibles [s coll]
  (filter #(< (Math/abs (- (count s) (count %))) 1) coll))
















