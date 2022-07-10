(ns temp.navigation
  (:require [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.vector :as vector]))

(defn point-below [world [x y z]]
  (let [raycaster (new THREE.Raycaster)
        point (new THREE.Vector3 x y z)
        ground (three/get-object world "ground")]
    (.updateMatrixWorld ground)
    (.set raycaster point (new THREE.Vector3 0 -1 0))
    (let [intersection (.intersectObject raycaster ground)]
      (if (empty? intersection)
        nil
        (-> intersection
            array-seq
            first
            js->clj
            (get "point")
            three/point->vector)))))

(defn move-helper [world axis-value direction velocity-name speed]
  (let [f 0.02
        velocity (get world velocity-name)
        new-velocity (if (> (util/abs axis-value) 0.1)
                       (util/within (+ velocity (* axis-value f -1)) -1 1)
                       (* velocity 0.9))
        position (get-in world [:camera :position])
        new-position (->> new-velocity
                          (* speed)
                          (vector/multiply direction)
                          (vector/add position))
        point (point-below world new-position)
        new-position (if point
                       new-position
                       position)]
    (-> world
        (assoc-in [:camera :position] new-position)
        (assoc-in [:camera :position 1] (+  (second point) 1.37))
        (assoc-in [velocity-name] new-velocity)
        (update-in [:camera] three/sync-object))))

(defn rotate [world axis-value speed]
  (let [f 0.04
        velocity (get world :rotate-velocity)
        new-velocity (if (> (util/abs axis-value) 0.1)
                       (util/within (+ velocity (* axis-value f -1)) (- speed) speed)
                       (* velocity 0.95))]
    (-> world
        (update-in [:camera :rotation 3] #(+ % (* new-velocity 0.6)))
        (assoc-in [:rotate-velocity] new-velocity)
        (update-in [:camera] three/sync-object))))

(defn move [world]
  (let [camera-direction (-> (three/get-camera-direction world)
                             (assoc-in [1] 0)
                             (vector/normalize))
        a (get-in world [:controllers :right :axes 1])
        d (get-in world [:controllers :right :axes 0])
        b (get-in world [:controllers :left :axes 1])
        c (get-in world [:controllers :left :axes 0])
        [b c] (if (> (util/abs b) (util/abs c))
                [b 0]
                [0 c])
        [a d] (if (> (util/abs a) (util/abs d))
                [a 0]
                [0 d])]
    (-> world
        (move-helper a camera-direction :velocity 0.015)
        ;; (move-helper b [0 1 0] :velocity2 0.01)
        (rotate c 1)
        (rotate d 1))))
