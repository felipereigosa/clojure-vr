(ns temp.core
  (:require [temp.library.camera :as camera]
            [temp.library.vector :as vector]
            [temp.library.meshes :as mesh]
            [temp.library.canvas :as canvas]
            [temp.library.util :as util]
            [temp.library.world :as world]
            [temp.library.matrix :as matrix]
            ))

(defn create-world [world]
  (.clearColor (:gl world) 0.0 0.5 0.8 1.0)
  (-> world
      (assoc-in [:camera] (camera/create 3 0 0))
      (update-in [:camera] #(camera/set-pivot % [0 0.5 0]))

      (assoc-in [:meshes :ground]
                (mesh/create-model "cube.obj" [0 -0.101 0] [1 0 0 0]
                                   [12 0.2 12] [40 40 40]))
      (assoc-in [:meshes :grid]
                (mesh/create-wireframe (mesh/get-grid-vertices 12 1)
                                       [0 0 0] [1 0 0 0] 1 :black))
      ;;---

      (assoc-in [:meshes :cube]
                (mesh/create-model "cube.obj" [-0.75 0.5 -0.75] [0 1 0 0]
                                   1 :red))

      (assoc-in [:meshes :cylinder]
                (mesh/create-model "cylinder.obj" [0.75 0.5 -0.75] [0 1 0 0]
                                   1 :yellow))

      (assoc-in [:meshes :cone]
                (mesh/create-model "cone.obj" [-0.75 0.5 0.75] [0 1 0 0]
                                   1 :green))

      (assoc-in [:meshes :sphere]
                (mesh/create-model "sphere.obj" [0.75 0.5 0.75] [0 1 0 0]
                                   1 :blue))

      (assoc-in [:meshes :controller-right]
                (mesh/create-model "cube.obj" [0 0 0] [0 1 0 0] 0.1 :red))

      (assoc-in [:meshes :controller-left]
                (mesh/create-model "cube.obj" [0 0 0] [0 1 0 0] 0.1 :white))

      (assoc-in [:meshes :point]
                (mesh/create-model "cube.obj" [0 0 0] [0 1 0 0] 0.01 :black))
      ))
;; (reset! world/reload? true)

(defn update-world [world]
  world
  )

(defn draw-world! [world]
  (doseq [mesh (vals (:meshes world))]
    (mesh/draw world mesh))
  )

(defn move-point [world]
  (let [left-cube (get-in world [:meshes :controller-left])
        rotation (:rotation left-cube)
        point (matrix/apply-transform (matrix/make-transform [0 0 0] rotation)
                                      [0 0 -0.1])]
    (-> world
        (assoc-in [:meshes :point :position]
                  (vector/add (:position left-cube) point))
        (assoc-in [:meshes :point :rotation] rotation))))

(defn snap-cube [world]
  (let [red-cube (get-in world [:meshes :controller-right])
        white-cube (get-in world [:meshes :controller-left])
        point (get-in world [:meshes :point])
        point-position (:position point)
        cube-position (:position red-cube)
        rotation (:rotation white-cube)]
    (if (< (vector/distance cube-position point-position) 0.3)
      (update-in world [:meshes :controller-right]
                 #(merge %
                         {:position point-position
                          :rotation rotation}))
      world)))

(defn button-pressed [world {:keys [hand button]}]
  (if (= button :grip)
    (assoc-in world [:meshes (util/join-keywords :controller hand) :color] '(1 0 0))
    world))

(defn button-released [world {:keys [hand button]}]
  (if (= button :grip)
    (assoc-in world [:meshes (util/join-keywords :controller hand) :color] '(1 1 1))
    world))

(defn controller-moved [world {:keys [position rotation hand] :as event}]
  (-> world
      (update-in [:meshes (util/join-keywords :controller hand)]
                 #(merge %
                         {:position (vector/add position [0 0.5 3])
                          :rotation rotation}))
      move-point
      snap-cube))

(defn keep-active? [world]
  true
  )
