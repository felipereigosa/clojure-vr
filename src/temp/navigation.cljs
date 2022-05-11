(ns temp.navigation
  (:require [temp.library.camera :as camera]
            [temp.library.vector :as vector]
            [temp.library.transform :as transform]
            [temp.library.util :as util]
            [temp.library.mesh :as mesh]
            [temp.controller :as controller]))

(defn draw-arc! [world]
  (when (get-in world [:arc :show])
    (mesh/draw world (:arc world))))

(defn get-gravity-parabola-points [points position direction]
  (if (neg? (second position))
    (conj points position)
    (recur (conj points position)
           (vector/add position direction)
           (vector/add direction [0 -0.001 0]))))

(defn compute-teleport-arc [world]
  (let [{:keys [position rotation]} (controller/get-transform world :right)
        direction (transform/apply {:rotation rotation
                                    :position [0 0 0]} [0 0 -1])
        points (get-gravity-parabola-points
                 []
                 position
                 (vector/multiply direction 0.08))
        vertices (mapcat (fn [a b]
                           [a b]) points (rest points))]
    (update-in world [:arc]
               #(merge % {:vertex-buffer (js/Float32Array. (flatten vertices))
                          :num-vertices (count vertices)
                          :last-point (last vertices)
                          :show true}))))
(defn jump [world]
  (let [arc (:arc world)]
    (if (:show arc)
      (let [[x _ z] (get-in world [:arc :last-point])]
        (-> world
                (update-in [:camera] #(camera/move % [(util/within x -6 6) 1 (util/within z -6 6)]))
            (assoc-in [:arc :show] false)))
      world)))

(defn move-back [world]
  (if (not (:moving world))
    (let [{:keys [position direction]} (:camera world)
          new-position (vector/add position (vector/multiply direction -0.3))]
      (-> world
          (update-in [:camera] #(camera/move % new-position))
          (assoc-in [:moving] true)))
    world))

(defn turn [world angle]
  (if (not (:moving world))
    (-> world
        (update-in [:camera] #(camera/rotate % angle))
        (assoc-in [:moving] true))
    world))

(defn move [world hand axes]
  (if (= hand :right)
    (cond
      (> (vector/dot-product axes [0 1]) 0.8) (compute-teleport-arc world)
      (> (vector/dot-product axes [1 0]) 0.8) (turn world -45)
      (> (vector/dot-product axes [-1 0]) 0.8) (turn world 45)
      (> (vector/dot-product axes [0 -1]) 0.8) (move-back world)
      :else (-> world
                (assoc-in [:moving] false)
                jump))
    world))
