(ns temp.core
  (:require [temp.library.camera :as camera]
            [temp.library.vector :as vector]
            [temp.library.meshes :as mesh]
            [temp.library.canvas :as canvas]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.world :as world]
            [temp.library.matrix :as matrix]
            ))

(defn create-world [world]
  (.clearColor (:gl world) 0.0 0.5 0.8 1.0)
  (-> world
      (assoc-in [:camera] (camera/create 0.7 0 0))
      (update-in [:camera] #(camera/set-pivot % [0 1 0]))

      (assoc-in [:background-meshes :ground]
                (mesh/create-model "cube.obj" [0 -0.101 0] [1 0 0 0]
                                   [12 0.2 12] [40 40 40]))
      (assoc-in [:background-meshes :grid]
                (mesh/create-wireframe (mesh/get-grid-vertices 12 1)
                                       [0 0 0] [1 0 0 0] 1 :black))
      ;;---

      (assoc-in [:background-meshes :controller-right]
                (mesh/create-model "cube.obj" [0 0 0] [0 1 0 0] 0.05 :red))

      (assoc-in [:background-meshes :controller-left]
                (mesh/create-model "cube.obj" [0 0 0] [0 1 0 0] 0.05 :white))

      (assoc-in [:background-meshes :table]
                (mesh/create-model "cube.obj" [0 0.5 0] [0 1 0 0]
                                   [1.5 0.05 1] :white))

      (assoc-in [:meshes :p1]
                (mesh/create-model "cube.obj" [0 (+ 0.5 0.025 0.05) 0.2] [0 1 0 0]
                                   0.1 :red))

      (assoc-in [:meshes :p2]
                (mesh/create-model "cube.obj" [0.2 (+ 0.5 0.025 0.05) 0.1] [0 1 0 30]
                                   0.1 :yellow))

      (assoc-in [:meshes :p3]
                (mesh/create-model "cylinder.obj" [-0.3 (+ 0.5 0.025 0.1) 0.1] [0 1 0 0]
                                   [0.15 0.2 0.15] :blue))

      (assoc-in [:meshes :p4]
                (mesh/create-model "cone.obj" [0.5 (+ 0.5 0.025 0.1) -0.2] [0 1 0 0]
                                   0.2 :green))

      (assoc-in [:meshes :p5]
                (mesh/create-model "sphere.obj" [-0.5 (+ 0.5 0.025 0.1) -0.2] [0 1 0 0]
                                   0.2 :orange))
      ))
(reset! world/reload? false)

(defn update-world [world]
  world
  )

(defn draw-world! [world]
  (doseq [mesh (vals (:background-meshes world))]
    (mesh/draw world mesh))

  (doseq [mesh (vals (:meshes world))]
    (mesh/draw world mesh))
  )

;; (defn snap-cube [world]
;;   (let [red-cube (get-in world [:meshes :controller-right])
;;         white-cube (get-in world [:meshes :controller-left])
;;         point (get-in world [:meshes :point])
;;         point-position (:position point)
;;         cube-position (:position red-cube)
;;         rotation (:rotation white-cube)]
;;     (if (< (vector/distance cube-position point-position) 0.3)
;;       (do
;;         (when (not (:snapped world))
;;           (.pulse (get-in world [:actuators :left]) 1 200)
;;           (.pulse (get-in world [:actuators :right]) 1 200)
;;           (println "snapped"))
;;         (-> world
;;             (update-in [:meshes :controller-right]
;;                        #(merge %
;;                                {:position point-position
;;                                 :rotation rotation}))
;;             (assoc-in [:snapped] true)))
;;       (assoc-in world [:snapped] false))))

(defn get-closest-object [meshes position]
  (->> meshes
       (map (fn [[name mesh]]
              [name (vector/distance (:position mesh) position)]))
       (sort-by second)
       (filter #(< (second %) 0.1))
       first
       first))

(defn button-pressed [world {:keys [hand button]}]
  (if (= button :grip)
    (let [actuator (get-in world [:actuators hand])
          controller (get-in world [:background-meshes (util/join-keywords :controller hand)])]
      (if-let [object-name (get-closest-object (:meshes world) (:position controller))]
        (let [object (get-in world [:meshes object-name])
              relative-transform (matrix/remove-transform object controller)]
          (.pulse actuator 1 50)
          (assoc-in world [:picked-object hand] (assoc relative-transform :name object-name)))
        world))
    world))

(defn button-released [world {:keys [hand button]}]
  (if (= button :grip)
    (dissoc-in world [:picked-object hand])
    world))

(defn move-picked-object [world hand]
  (if-let [picked-object (get-in world [:picked-object hand])]
    (let [controller (get-in world [:background-meshes (util/join-keywords :controller hand)])
          name (:name picked-object)
          transform (matrix/combine-transforms picked-object controller)]
      (-> world
          (assoc-in [:meshes name :position] (:position transform))
          (assoc-in [:meshes name :rotation] (:rotation transform))))
    world))

(defn controller-moved [world {:keys [position rotation hand] :as event}]
  (-> world
      (update-in [:background-meshes (util/join-keywords :controller hand)]
                 #(merge %
                         {:position (vector/add position [0 1 0.7])
                          :rotation rotation}))
      (move-picked-object :right)
      (move-picked-object :left)
      ;; snap-cube
      ))

(defn keep-active? [world]
  true)
