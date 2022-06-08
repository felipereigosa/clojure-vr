(ns temp.core
  (:require [temp.library.world :as world]
            [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.vector :as vector]
            [temp.library.transform :as transform]
            [temp.library.controllers :as controllers]
            [temp.manipulation :as manipulation]))

(defn create-world [world]
  (-> world
      (three/set-clear-color [0 128 204])
      (three/create-lights)
      (assoc-in [:camera :position] [0 1 2])
      (update-in [:camera] three/sync-object)
      (three/create-cube [:background-meshes :ground]
                         [0 -0.101 0] [1 0 0 0] [12 0.2 12] [5 5 5])
      (three/create-wireframe [:background-meshes :grid]
                              (three/get-grid-vertices 12 1) [2 2 2] 2)
      (three/create-cube [:controllers :right] [0.1 0 0] [0 1 0 0] 0.05 :red)
      (three/create-cube [:controllers :left] [-0.1 0 0] [0 1 0 0] 0.05 :white)
      (three/create-cube [:meshes :cube] [0 0.5 0] [0 1 0 30] 1 :red)
      (three/create-model [:meshes :balls] "balls.glb" [0 0 0] [0 1 0 0] 1)
      ))
(reset! world/reload? false)

(defn update-world [world]
  (-> world
      (controllers/update)
      (manipulation/move :right)
      (manipulation/move :left)
      ))

(defn create-and-grab [world hand]
  (let [{:keys [position rotation]} (get-in world [:controllers hand])
        scale (vector/multiply [24 9.6 16] 0.02)
        name (util/gen-keyword :brick)
        colors [:red :green :blue :yellow]
        color (rand-nth colors)]
    (three/pulse world hand 1 50)
    (-> world
        (three/create-cube [:meshes name] position rotation scale color)
        (assoc-in [:picked-object hand] {:position [0 0 0]
                                         :rotation [1 0 0 0]
                                         :name name}))))
(defn remove-all-bricks [world]
  (let [scene (:scene world)]
    (doseq [brick (vals (:meshes world))]
      (.remove scene (:object brick))))
  (dissoc-in world [:meshes]))

(defn button-pressed [world {:keys [hand button]}]
  (cond
    (= button :grip) (manipulation/grab world hand)
    (= button :trigger) (create-and-grab world hand)
    (= button :a) (remove-all-bricks world)
    :else world))

(defn button-released [world {:keys [hand button]}]
  (if (= button :grip)
    (manipulation/release world hand)
    world))
