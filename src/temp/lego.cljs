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

(defn create-new-brick [world name position rotation color]
  (let [brick (:brick world)
        new-brick (.clone (:group brick))
        mesh (first (array-seq (.-children new-brick)))]
    (set! (.-material mesh) (get (:materials brick) color))
    (.add (:scene world) new-brick)
    (assoc-in world [:meshes name]
              (three/sync-object
                {:object new-brick
                 :position position
                 :rotation rotation
                 :scale 1}))))

(defn create-bricks [world]
  (let [xs [-0.231853 -0.077284 0.077284 0.231853]
        zs [-0.077284 0.077284]
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
                          (util/create-combinations xs zs))
        snaps (vec (concat top-snaps bottom-snaps))]
    (-> world
        (create-new-brick :blue-brick [0 5.8 1] [1 0 0 0] :blue)
        (update-in [:meshes :blue-brick] #(merge % {:snaps snaps}))
        (physics/create-cube [:meshes :blue-brick] 1 [0.618274 0.185482 0.309138])

        (create-new-brick :yellow-brick [0.154 (+ 3.8 0.185) 1.154] [0 1 0 90] :yellow)
        (update-in [:meshes :yellow-brick] #(merge % {:snaps snaps}))
        (physics/create-cube [:meshes :yellow-brick] 1 [0.618274 0.185482 0.309138])

        (create-new-brick :red-brick [0.154 1.8 1.154] [0 1 0 90] :red)
        (update-in [:meshes :red-brick] #(merge % {:snaps snaps}))
        (physics/create-cube [:meshes :red-brick] 1 [0.618274 0.185482 0.309138]))))

(defn load-brick-helper [world brick-group]
  (let [brick (first (array-seq (.-children brick-group)))
        material (.-material brick)
        materials (reduce merge {:red material}
                          (map (fn [color]
                                 (let [new-material (.clone material)]
                                   (set! (.-color new-material) (three/get-color color))
                                   {color new-material}))
                               [:green :yellow :blue]))]
    (-> world
        (assoc-in [:brick :group] brick-group)
        (assoc-in [:brick :materials] materials))))

(defn load-brick [w]
  (let [loader (new js/window.GLTFLoader)]
    (.load
      loader
      "brick.glb"
      (fn [gltf]
        (swap! world/world load-brick-helper (.-scene gltf))
        (swap! world/world create-bricks))))
  w)

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
        load-brick
        (three/create-model [:background-meshes :room] "baked.glb" [0 -0.18 0] [0 1 0 180] 1)
        (three/create-cube [:collision-cube] [0 1.5 1] [0 0 1 0] 0.2 :red)
        (update-in [:collision-cube] #(three/set-visible % false))
        )))

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
