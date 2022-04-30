(ns temp.library.camera
  (:require [temp.library.util :as util]
            [temp.library.vector :as vector]
            [temp.library.matrix :as matrix]
            [temp.library.analytic-geometry :as ag]))

(defn compute-view-matrix [camera]
  (let [pivot (:pivot camera)
        eye (-> [0 0 1]
                (vector/multiply (:distance camera))
                (vector/rotate [1 0 0] (- (:x-angle camera)))
                (vector/rotate [0 1 0] (- (:y-angle camera)))
                (vector/add pivot))
        view-matrix (matrix/look-at eye pivot [0 1 0])]
    (-> camera
        (assoc-in [:view-matrix] view-matrix)
        (assoc-in [:eye] eye))))

(defn set-pivot [camera pivot]
  (-> camera
      (assoc-in [:pivot] pivot)
      compute-view-matrix))

(defn create [distance x-angle y-angle]
  (compute-view-matrix {:distance distance
                        :x-angle x-angle
                        :y-angle y-angle
                        :pivot [0 0 0]}))

(defn rotate [camera dx dy]
  (let [x-speed 0.5
        y-speed 0.5
        camera (-> camera
                   (update-in [:x-angle]
                              (fn [angle]
                                (util/within (+ angle (* dy y-speed)) -89 89)))
                   (update-in [:y-angle] (fn [angle] (+ angle (* dx y-speed)))))]
    (compute-view-matrix camera)))

(defn unproject-point [world [x y]]
  (let [dx (dec (/ x (/ (:screen-width world) 2)))
        dy (- (dec (/ y (/ (:screen-height world) 2))))
        p-matrix (:projection-matrix world)
        v-matrix (get-in world [:camera :view-matrix])
        m (matrix/multiply v-matrix p-matrix)
        im (matrix/invert m)
        to3d (fn [v]
               (as-> v p
                 (matrix/multiply im p)
                 (map #(/ % (nth p 3)) p)
                 (butlast p)
                 (vec p)))
        p-a (to3d [dx dy -1.0 1.0])
        p-b (to3d [dx dy 0.0 1.0])]
    [p-a (vector/normalize (vector/subtract p-b p-a))]))

(defn pan [camera world x1 y1 x2 y2]
  (let [l1 (unproject-point world [x1 y1])
        l2 (unproject-point world [x2 y2])
        plane [[0 0 0] [0 0 1] [1 0 0]]
        p1 (ag/line-plane-intersection l1 plane)
        p2 (ag/line-plane-intersection l2 plane)
        d (vector/subtract p1 p2)]
    (-> camera
        (update-in [:pivot] #(vector/add % d))
        compute-view-matrix)))

(defn zoom [camera amount]
  (-> camera
      (update-in [:distance] #(* % amount))
      compute-view-matrix))
