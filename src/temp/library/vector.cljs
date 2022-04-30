(ns temp.library.vector
  (:require [temp.library.util :as util]
            [temp.library.matrix :as matrix]))

(defn dot-product [v1 v2]
  (reduce + (map * v1 v2)))

(defn length [v]
  (Math/sqrt (dot-product v v)))

(defn add [v1 v2]
  (vec (map + v1 v2)))

(defn subtract [v1 v2]
  (vec (map - v1 v2)))

(defn multiply [v amount]
  (vec (map (partial * amount) v)))

(defn cross-product [a b]
  (let [[a0 a1 a2] a
        [b0 b1 b2] b]
    [(- (* a1 b2) (* a2 b1))
     (- (* a2 b0) (* a0 b2))
     (- (* a0 b1) (* a1 b0))]))

(defn normalize [v]
  (vec (map #(/ % (length v)) v)))

(defn rotate [vector axis angle]
  (let [rotation (matrix/from-rotation axis angle)]
    (vec (butlast (matrix/multiply rotation vector)))))

(defn distance [a b]
  (length (subtract a b)))

(defn scalar-projection [a b]
  (dot-product a (normalize b)))
