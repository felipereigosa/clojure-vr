(ns temp.library.vector
  (:require [temp.library.util :as util]
            [temp.library.transform :as transform]))

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
  (let [rotation {:position [0 0 0]
                  :rotation (conj axis angle)}]
    (transform/apply rotation vector)))

(defn rotate-around [point axis center angle]
  (-> point
      (subtract center)
      (rotate axis angle)
      (add center)))

(defn distance [a b]
  (length (subtract a b)))

(defn scalar-projection [a b]
  (dot-product a (normalize b)))

(defn equals? [a b]
  (every? #(util/float= % 0.0) (subtract a b)))

(defn angle [a b]
  (let [v (/ (dot-product a b)
             (* (length a) (length b)))]
    (cond
      (>= v 1) 0
      (<= v -1) 180
      :else (Math/acos v))))

(defn direction->rotation [direction]
  (let [direction (normalize direction)]
    (cond
      (equals? [0 1 0] direction) [1 0 0 0]
      (equals? [0 -1 0] direction) [1 0 0 180]
      :else (let [axis (cross-product [0 1 0] direction)
                  angle (angle [0 1 0] direction)]
              (conj axis (util/to-degrees angle))))))
