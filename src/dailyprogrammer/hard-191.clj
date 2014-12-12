(ns dailyprogrammer.hard-191
  (:require [clojure.string :as str]))

(defn make-line [n x1 y1 x2 y2]
  (let [[x1 y1 x2 y2] (if (<= x1 x2)
                        [x1 y1 x2 y2]
                        [x2 y2 x1 y1])
        m             (if (not= x1 x2)
                        (/ (- y2 y1)
                           (- x2 x1)))
        c             (if (not= x1 x2)
                        (- y2
                           (* m x2)))]
    {:id          n
     :left        [x1 y1]
     :right       [x2 y2]
     :slope       m
     :y-intercept c}))

(defn point-above-line? [[x y] line]
  (let [{[x1 y1] :left
         [x2 y2] :right
         m       :slope
         c       :y-intercept} line]
    (if m
      (and (> (- y (* m x) c) 0)
           (>= x x1)
           (<= x x2))
      (> y (max y1 y2) (min y1 y2)))))

(defn line-above? [p q]
  (let [{p1 :left p2 :right} p]
    (or (point-above-line? p1 q)
        (point-above-line? p2 q))))

(defn highest-line [lx]
  (if-not (empty? lx)
    (loop [l1 nil lx lx]
      (let [[l2 & lx] lx]
        (cond (nil? l1)           (recur l2 lx)
              (nil? l2)           l1
              (line-above? l1 l2) (recur l1 lx)
              (line-above? l2 l1) (recur l2 lx)
              :default            (recur l1 lx))))))

(defn order-lines [lx]
  (loop [result [] lx lx]
    (let [l (highest-line lx)]
      (if l
        (recur (conj result l) (remove #(= l %) lx))
        result))))

(defn line-ids [lx]
  (map :id lx))

(defn extract-line-args [line]
  (let [pat (re-pattern (->> "(\\d+(?:\\.\\d+)?)"
                             (repeat 4)
                             (interpose ",")
                             (cons "(\\d+):")
                             (apply str)))
        matches (re-find pat line)
        matches (rest matches)]
    (cons (Integer. (first matches))
          (map #(Double. %) (rest matches)))))

(defn read-input [path]
  (let [lines ((comp rest str/split-lines slurp) path)]
    (map #(apply make-line (extract-line-args %)) lines)))

(defn solve [path]
  (-> path
      read-input
      order-lines
      line-ids))
