(ns temp.lego
  (:require [temp.library.world :as world]
            [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.vector :as vector]
            [temp.library.transform :as transform]
            [temp.library.controllers :as controllers]
            [temp.library.physics :as physics]
            [temp.manipulation :as manipulation]
            [temp.navigation :as navigation]))

(defn get-snaps [[x z]]
  (let [get-coords (fn [size]
                     (map (fn [n]
                            (* (- n (/ (dec size) 2.0)) 0.154568))
                          (range size)))
        xs (get-coords x)
        zs (get-coords z)
        angles [0 90 180 270]
        top-snaps (map (fn [[x z angle]]
                         {:position [x 0.0925 z]
                          :rotation [0 1 0 angle]
                          :type :other})
                       (util/create-combinations xs zs angles))
        bottom-snaps (map (fn [[x z]]
                            {:position [x -0.0925 z]
                             :rotation [1 0 0 180]
                             :type :other})
                          (util/create-combinations xs zs))]
    (vec (concat top-snaps bottom-snaps))))

(defn create-brick [world which name position rotation color]
  (let [[x z] which
        brick (.getObjectByName (:bricks world) (str x "x" z))
        mesh (.clone brick)
        dimensions [(* 0.1545685 x) 0.185482 (* 0.1545685 z)]
        mass (reduce + dimensions)
        snaps (get-snaps which)]
    (set! (.-material mesh) (get (:materials world) color))
    (.add (:scene world) mesh)
    (-> world
        (assoc-in [:meshes name]
                  (three/sync-object
                    {:object mesh
                     :position position
                     :rotation rotation
                     :scale 1
                     :dimensions dimensions
                     :snaps snaps}))
        (physics/create-cube [:meshes name] mass dimensions))))

(defn create-bricks [world]
  (-> world
      (create-brick [2 2] :brick-0 [0 5.8 1] [1 0 0 0] :blue)
      (create-brick [4 2] :brick-1 [0.154 (+ 3.8 0.185) 1.154] [0 1 0 90] :yellow)
      (create-brick [2 2] :brick-2 [0.154 1.8 1.154] [0 1 0 90] :red)
      (create-brick [6 1] :brick-3 [1 5.8 1] [1 0 0 0] :green)
      (create-brick [8 1] :brick-4 [2 5.8 1] [1 0 0 0] :blue)
      (create-brick [3 1] :brick-5 [-1 5.8 1] [1 0 0 0] :yellow)
      (create-brick [1 1] :brick-6 [-2 5.8 1] [1 0 0 0] :red)
      (create-brick [2 1] :brick-7 [1 5.8 2] [1 0 0 0] :green)
      (create-brick [8 2] :brick-8 [2 5.8 2] [1 0 0 0] :yellow)
      (create-brick [3 2] :brick-9 [-1 5.8 2] [1 0 0 0] :green)))

(defn create-materials [world brick-scene]
  (let [material (.-material (first (array-seq (.-children brick-scene))))
        materials (reduce merge {:red material}
                          (map (fn [color]
                                 (let [new-material (.clone material)]
                                   (set! (.-color new-material) (three/get-color color))
                                   {color new-material}))
                               [:green :yellow :blue]))]
    (assoc-in world [:materials] materials)))

(defn load-bricks [w]
  (let [loader (new js/window.GLTFLoader)]
    (.load loader "bricks.glb"
           (fn [gltf]
             (swap! world/world create-materials (.-scene gltf))
             (swap! world/world assoc-in [:bricks] (.-scene gltf))
             (swap! world/world create-bricks)))
    w))

(defn create-world [world]
  (let [planet (physics/create-planet 10)]
    (-> world
        (assoc-in [:planet] planet)
        (three/set-clear-color [0 128 204])
        (three/create-lights [1 1 1])
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
        load-bricks
        (three/create-model [:background-meshes :room] "baked.glb" [0 -0.18 0] [0 1 0 180] 1)
        (three/create-cube [:collision-cube] [0 1.5 1] [0 0 1 0] 0.2 :red)
        (update-in [:collision-cube] #(three/set-visible % false)))))

(defn sync-bodies [world]
  (reduce (fn [w [mesh-name mesh]]
            (if (or (:snapped mesh)
                    (= mesh-name (get-in world [:picked-object :left :name]))
                    (= mesh-name (get-in world [:picked-object :right :name])))
              (update-in w [:meshes mesh-name] physics/inverse-body-sync)
              (update-in w [:meshes mesh-name] physics/body-sync)))
          world
          (:meshes world)))

(defn update-world [world]
  (-> world
      controllers/update
      (manipulation/move :right)
      (manipulation/move :left)
      navigation/move
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
