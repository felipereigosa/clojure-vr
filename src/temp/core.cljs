(ns temp.core
  (:require [temp.library.world :as world]
            [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.vector :as vector]
            [temp.library.transform :as transform]
            [temp.library.controllers :as controllers]
            [temp.library.physics :as physics]
            [temp.manipulation :as manipulation]
            [temp.navigation :as navigation]
            [temp.lego :as lego]))

(defn create-world [world]
  (let [planet (physics/create-planet 10)]
    (-> world
        (assoc-in [:planet] planet)
        (three/set-clear-color [0 128 204])
        (three/create-lights)
        (assoc-in [:camera :position] [0 0.6 2])
        (assoc-in [:camera :rotation] [0 1 0 0])
        (update-in [:camera] three/sync-object)
        (three/create-cube [:background-meshes :ground]
                           [0 -0.101 0] [1 0 0 0] [12 0.2 12] [5 5 5])
        (physics/create-floor [:background-meshes :ground])
        (three/create-wireframe [:background-meshes :grid]
                                (three/get-grid-vertices 12 1) [2 2 2] 2)
        (three/create-cube [:controllers :right] [0.1 0 0] [0 1 0 0] 0.05 :red)
        (three/create-cube [:controllers :left] [-0.1 0 0] [0 1 0 0] 0.05 :white)
        lego/load-brick
        (three/create-model [:background-meshes :room] "baked.glb" [0 -0.18 0] [0 1 0 180] 1)
        (three/create-cube [:collision-cube] [0 1.5 1] [0 0 1 0] 0.2 :red)
        (update-in [:collision-cube] #(three/set-visible % false))

        (three/create-cube [:meshes :block] [0 50 0] [0 0 1 0] 0.2 :red)
        (physics/create-cube [:meshes :block] 1))))

(defn sync-bodies [world]
  (reduce (fn [w mesh-name]
            (if (and (not (= mesh-name (get-in world [:picked-object :left :name])))
                     (not (= mesh-name (get-in world [:picked-object :right :name]))))
              (update-in w [:meshes mesh-name] physics/body-sync)
              (update-in w [:meshes mesh-name] physics/inverse-body-sync)))
          world
          (keys (:meshes world))))

(defn update-world [world]
  (-> world
      controllers/update
      (manipulation/move :right)
      (manipulation/move :left)
      navigation/move
      navigation/turn
      physics/step
      sync-bodies))

(defn button-pressed [world {:keys [hand button]}]
  (cond
    (= button :grip) (manipulation/grab world hand)
    :else world))

(defn button-released [world {:keys [hand button]}]
  (if (= button :grip)
    (manipulation/release world hand)
    world))

(defn mouse-scrolled [world event]
  world)

(defn mouse-pressed [world event]
  world)

(defn mouse-moved [world event]
  world)

(defn mouse-released [world event]
  world)
