(ns temp.core
  (:require [temp.library.camera :as camera]
            [temp.library.vector :as vector]
            [temp.library.mesh :as mesh]
            [temp.library.canvas :as canvas]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.world :as world]
            [temp.library.matrix :as matrix]
            [temp.library.transform :as transform]
            [temp.controller :as controller]
            [temp.navigation :as navigation]
            [temp.manipulation :as manipulation]))

(defn create-world [world]
  (.clearColor (:gl world) 0.0 0.5 0.8 1.0)
  (-> world
      (assoc-in [:camera] (camera/create [0 1 0.7] [0 1 0 0]))

      (assoc-in [:background-meshes :ground]
                (mesh/create-model "cube.obj" [0 -0.101 0] [1 0 0 0]
                                   [12 0.2 12] [40 40 40]))
      (assoc-in [:background-meshes :grid]
                (mesh/create-wireframe (mesh/get-grid-vertices 12 1)
                                       [0 0 0] [1 0 0 0] 1 :black))
      ;;---

      (assoc-in [:controller :right]
                (mesh/create-model "cube.obj" [0 0 0] [0 1 0 0] 0.05 :red))

      (assoc-in [:controller :left]
                (mesh/create-model "cube.obj" [0 0 0] [0 1 0 0] 0.05 :white))

      (assoc-in [:background-meshes :table]
                (mesh/create-model "cube.obj" [0 0.5 0] [0 1 0 0]
                                   [1.5 0.05 1] :white))

      ;;---

      (assoc-in [:meshes :red-cube]
                (merge (mesh/create-model "cube.obj" [0.2 (+ 0.5 0.025 0.05) 0.2]
                                          [1 0 0 0] 0.1 :red)
                        {:snaps [{:position [0.05 0 0]
                                 :rotation [0 0 1 -90]
                                 :type :point}
                                {:position [-0.05 0 0]
                                 :rotation [0 0 1 90]
                                 :type :point}
                                {:position [0 0.05 0]
                                 :rotation [1 0 0 0]
                                 :type :point}
                                {:position [0 -0.05 0]
                                 :rotation [1 0 0 180]
                                 :type :point}
                                {:position [0 0 0.05]
                                 :rotation [1 0 0 90]
                                 :type :point}
                                {:position [0 0 -0.05]
                                 :rotation [1 0 0 -90]
                                 :type :point}
                                ]}))

      (assoc-in [:meshes :yellow-cube]
                (merge (mesh/create-model "cube.obj" [0.2 (+ 0.5 0.025 0.05) 0]
                                          [0 1 0 30] 0.1 :yellow)
                       {:snaps [{:position [0.05 0 0]
                                 :rotation [0 0 1 -90]
                                 :type :point}
                                {:position [-0.05 0 0]
                                 :rotation [0 0 1 90]
                                 :type :point}
                                {:position [0 0.05 0]
                                 :rotation [1 0 0 0]
                                 :type :point}
                                {:position [0 -0.05 0]
                                 :rotation [1 0 0 180]
                                 :type :point}
                                {:position [0 0 0.05]
                                 :rotation [1 0 0 90]
                                 :type :point}
                                {:position [0 0 -0.05]
                                 :rotation [1 0 0 -90]
                                 :type :point}
                                ]}))

      (assoc-in [:meshes :green-cone]
                (merge (mesh/create-model "cone.obj" [0.4 (+ 0.5 0.025 0.05) 0.2]
                                          [1 0 0 0] 0.1 :green)
                       {:snaps [{:position [0 -0.05 0]
                                 :rotation [1 0 0 180]
                                 :type :direction}]}))

      (assoc-in [:meshes :yellow-cone]
                (merge (mesh/create-model "cone.obj"
                                          [0.4 (+ 0.5 0.025 0.05) 0]
                                          [0 1 0 30]
                                          0.1 :yellow)
                       {:snaps [{:position [0 -0.05 0]
                                 :rotation [1 0 0 180]
                                 :type :direction}]}))

      (assoc-in [:meshes :cylinder]
                (merge (mesh/create-model "cylinder.obj" [-0.3 (+ 0.5 0.025 0.1) 0.1]
                                          [1 0 0 0] [0.15 0.2 0.15] :blue)
                       {:snaps [{:position [0 0.1 0]
                                 :rotation [1 0 0 0]
                                 :type :direction}
                                {:position [0 -0.1 0]
                                 :rotation [1 0 0 180]
                                 :type :direction}]}))

      (assoc-in [:meshes :sphere]
                (merge (mesh/create-model "sphere.obj" [0.5 0 -3.5]
                                          [1 0 0 0] 0.1 :orange)
                       {:snaps [{:position [0 0 0]
                                 :rotation [1 0 0 0]
                                 :type :point}]}))

      ;;----------------------------------------------------------------------;;

      (assoc-in [:axis] (mesh/create-model "axis.obj" [0 0 0] [1 0 0 0] 1 nil))
      (assoc-in [:arc] (mesh/create-wireframe [] [0 0 0] [1 0 0 0] 1 :green))
      ))
(reset! world/reload? true)

(defn update-world [world]
  world)

(defn draw-world! [world]
  (doseq [mesh (vals (:background-meshes world))]
    (mesh/draw world mesh))

  (doseq [mesh (vals (:meshes world))]
    (mesh/draw world mesh))

  (when (nil? (get-in world [:picked-object :left]))
    (controller/draw! world :left))

  (when (nil? (get-in world [:picked-object :right]))
    (controller/draw! world :right))

  (navigation/draw-arc! world)
  (manipulation/draw-ghost-object! world))

(defn button-pressed [world {:keys [hand button]}]
  (if (= button :grip)
    (manipulation/grab world hand)
    world))

(defn button-released [world {:keys [hand button]}]
  (if (= button :grip)
    (manipulation/release world hand)
    world))

(defn controller-moved [world {:keys [position rotation hand axes] :as event}]
  (-> world
      (navigation/move hand axes)
      (controller/set-transform hand position rotation)
      (manipulation/move :left)
      (manipulation/move :right)
      (manipulation/snap-object)))
