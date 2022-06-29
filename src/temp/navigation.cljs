(ns temp.navigation
  (:require [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.vector :as vector]))

(defn move [world]
  (let [v (get-in world [:controllers :right :axes 1])
        f 0.02
        velocity (:velocity world)
        new-velocity (if (> (util/abs v) 0.1)
                       (util/within (+ velocity (* v f -1)) -1 1)
                       (* velocity 0.9))
        camera-direction (-> (three/get-camera-direction world)
                             (assoc-in [1] 0)
                             (vector/normalize))]
    (-> world
        (update-in [:camera :position]
                   #(->> new-velocity
                         (* 0.015)
                         (vector/multiply camera-direction)
                         (vector/add %)))
        (assoc-in [:velocity] new-velocity)
        (update-in [:camera] three/sync-object))))

(defn turn [world]
  (let [v (get-in world [:controllers :right :axes 0])
        pressed (> (util/abs v) 0.8)]
    (if (:turning world)
      (if pressed
        world
        (assoc-in world [:turning] false))
      (if pressed
        (-> world
            (update-in [:camera :rotation 3] #(+ % (if (pos? v) -30 30)))
            (update-in [:camera] three/sync-object)
            (assoc-in [:turning] true))
        world))))
