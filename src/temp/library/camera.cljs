(ns temp.library.camera
  (:require [temp.library.util :as util]
            [temp.library.three :as three]
            [temp.library.vector :as vector]
            [temp.library.transform :as transform]
            [temp.library.world :as world]))

(defn compute [world]
  (let [{:keys [distance x-angle y-angle pivot]} (:camera world)
        position (-> [0 0 1]
                     (vector/multiply distance)
                     (vector/rotate [1 0 0] (- x-angle))
                     (vector/rotate [0 1 0] (- y-angle))
                     (vector/add pivot))
        x-rotation {:position [0 0 0] :rotation [1 0 0 (- x-angle)]}
        y-rotation {:position [0 0 0] :rotation [0 1 0 (- y-angle)]}
        rotation (:rotation (transform/combine y-rotation x-rotation))]
    (-> world
        (assoc-in [:camera :position] position)
        (assoc-in [:camera :rotation] rotation)
        (update-in [:camera] three/sync-object))))

(defn transparent-ground [world]
  (let [ground (get-in world [:background-meshes :ground])
        visible (pos? (get-in world [:camera :x-angle]))]
    (if (not= visible (.-visible (:object ground)))
      (update-in world [:background-meshes :ground]
                 #(three/set-visible % visible))
      world)))

(defn rotate [world event]
  (let [[x y] (:last-point world)
        dx (- (:x event) x)
        dy (- (:y event) y)
        x-speed 0.4
        y-speed 0.4
        {:keys [x-angle y-angle]} (:camera world)
        new-x-angle (util/within (+ x-angle (* dy y-speed)) -89 89)
        new-y-angle (+ y-angle (* dx y-speed))]
    (-> world
        (assoc-in [:camera :x-angle] new-x-angle)
        (assoc-in [:camera :y-angle] new-y-angle)
        compute
        (assoc-in [:last-point] [(:x event) (:y event)])
        transparent-ground)))

(defn get-position [world]
  (let [camera (get-in world [:camera :camera])]
    (three/point->vector (.-position camera))))

(defn get-direction [world]
  (let [camera (get-in world [:camera :camera])
        vector (new THREE.Vector3 0 0 -1)]
    (.applyQuaternion vector (.-quaternion camera))
    (three/point->vector vector)))

(defn zoom [world amount]
  (-> world
      (update-in [:camera :distance]
                 #(util/within (- % amount) 2 30))
      compute
      world/redraw))
