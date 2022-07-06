(ns temp.room
  (:require [temp.library.world :as world]
            [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.vector :as vector]
            [temp.library.transform :as transform]
            [temp.library.controllers :as controllers]
            [temp.library.physics :as physics]
            [temp.manipulation :as manipulation]))

(defn create-world [world]
  (let [planet (physics/create-planet 10)]
    (-> world
        (assoc-in [:planet] planet)
        (three/set-clear-color [0 128 204])
        (three/create-lights)
        (assoc-in [:camera :position] [1 1.15 -1])
        (assoc-in [:camera :rotation] [0 1 0 0])
        (update-in [:camera] three/sync-object)
        (three/create-cube [:controllers :right] [0.1 0 0] [0 1 0 0] 0.05 :red)
        (three/create-cube [:controllers :left] [-0.1 0 0] [0 1 0 0] 0.05 :white)
        (three/create-model [:background-meshes :room] "room.glb" [0 0 0] [0 1 0 0] 1)
        )))

(defn change-angle [world]
  (if-let [angle (:angle world)]
    (-> world
        (update-in [:background-meshes :room :rotation 3] #(+ % angle))
        (update-in [:background-meshes :room] three/sync-object))
    world))

(defn update-world [world]
  (-> world
      controllers/update
      change-angle
      ))

(defn calibrate [world]
  (if (:calibrated world)
    world
    (let [controller (get-in world [:controllers :right])
          [x _ z] (:position controller)
          offset (vector/add [x 0 z] [0 0 0.05])]
      (-> world
          (assoc-in [:background-meshes :room :position] offset)
          (update-in [:background-meshes :room] three/sync-object)
          (assoc-in [:calibrated] true)
          ))))

(defn button-pressed [world {:keys [hand button]}]
  (case button
    :trigger (calibrate world)
    :a (assoc-in world [:angle] 0.05)
    :b (assoc-in world [:angle ] -0.05)
    world))

(defn button-released [world {:keys [hand button]}]
  (dissoc-in world [:angle]))
