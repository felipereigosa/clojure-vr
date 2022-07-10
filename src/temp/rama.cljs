(ns temp.rama
  (:require [temp.library.world :as world]
            [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.vector :as vector]
            [temp.library.transform :as transform]
            [temp.library.controllers :as controllers]
            [temp.navigation :as navigation]))

(defn assign-meshes [world]
  (let [scene (get-in world [:background-meshes :room :object])
        handle (three/get-object scene "handle")
        handle-collision (three/get-object scene "handle-collision")
        chamber (three/get-object scene "chamber")]
    (set! (.-visible handle-collision) false)
    (-> world
        (three/assign-mesh [:meshes :handle] handle)
        (three/assign-mesh [:meshes :chamber] chamber))))

(defn create-world [world]
  (let []
    (-> world
        (three/set-clear-color [0 0 0])
        (three/create-lights [1 1 1])
        (assoc-in [:camera :position] [0 0.6 2])
        (assoc-in [:camera :rotation] [0 1 0 0])
        (update-in [:camera] three/sync-object)
        (three/create-cube [:controllers :right] [0.1 0 0] [0 1 0 0] 0.05 :red)
        (three/create-cube [:controllers :left] [-0.1 0 0] [0 1 0 0] 0.05 :white)
        (three/create-model [:background-meshes :room] "rama.glb"
                            [0 0 0] [0 1 0 0] 1 assign-meshes))))

(defn rotate-handle [world]
  (if-let [handle-name (get-in world [:picked-object :right :name])]
    (let [controller (get-in world [:controllers :right])
          y (get-in controller [:position 1])
          angle (-> (:position controller)
                    (assoc-in [1] 0)
                    (vector/angle [1 0 0] [0 -1 0]))
          handle (get-in world [:meshes :handle])
          old-height (get-in handle [:position 1])
          old-angle (get-in handle [:rotation 3])]
      (-> world
          (assoc-in [:meshes :handle :position 1]
                    (if (< (util/sin (util/abs old-angle)) 0.1)
                      (util/within y 0 0.5)
                      old-height))
          (assoc-in [:meshes :handle :rotation 3]
                    (if (< old-height 0.5)
                      0
                      angle))
          (update-in [:meshes :handle] three/sync-object)))
    world))

(defn rotate-chamber [world]
  (if-let [handle-name (get-in world [:picked-object :right :name])]
    (let [angle (* (get-in world [:meshes :handle :rotation 3]) 1)]
      (-> world
          (assoc-in [:meshes :chamber :rotation] [0 0 1 angle])
          (update-in [:meshes :chamber] three/sync-object)))
    world))

(defn update-world [world]
  (-> world
      controllers/update
      navigation/move
      rotate-handle
      rotate-chamber))

(defn grab [world hand]
  (let [scene (get-in world [:background-meshes :room :object])
        point (get-in world [:controllers hand :position])
        box (three/get-object scene "handle-collision")]
    (if-let [object-name (three/inside-object? box point)]
      (assoc-in world [:picked-object hand :name] object-name)
      world)))

(defn button-pressed [world {:keys [hand button]}]
  (cond
    (= button :grip) (grab world hand)
    :else world))

(defn button-released [world {:keys [hand button]}]
  (if (= button :grip)
    (dissoc-in world [:picked-object hand])
    world))
