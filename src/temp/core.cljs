(ns temp.core
  (:require [temp.library.world :as world]
            [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.vector :as vector]
            [temp.library.transform :as transform]
            [temp.library.controllers :as controllers]
            [temp.library.physics :as physics]
            [temp.manipulation :as manipulation]))

(defn process-brick [world brick-group]
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

(defn load-brick [w loaded]
  (let [loader (new js/window.GLTFLoader)]
    (.load
      loader
      "brick.glb"
      (fn [gltf]
        (swap! world/world process-brick (.-scene gltf))
        (swap! world/world loaded)
        )))
  w)

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
        (create-new-brick :blue-brick [0 0.8 1] [1 0 0 0] :blue)
        (update-in [:meshes :blue-brick] #(merge % {:snaps snaps}))
        (create-new-brick :yellow-brick [0.154 (+ 0.8 0.185) 1.154] [0 1 0 90] :yellow)
        (update-in [:meshes :yellow-brick] #(merge % {:snaps snaps})))))

(defn create-world [world]
  (let [planet (physics/create-planet 10)]
    (-> world
        (assoc-in [:planet] planet)
        (three/set-clear-color [0 128 204])
        (three/create-lights)
        ;; (assoc-in [:camera :position] [0 1.3 2])
        ;; (assoc-in [:camera :rotation] [0 1 0 0])
        ;; (update-in [:camera] three/sync-object)
        ;; (three/set-camera [0 2 4] [0 0.5 0])
        (three/create-cube [:background-meshes :ground]
                           [0 -0.101 0] [1 0 0 0] [12 0.2 12] [5 5 5])
        (physics/create-floor [:background-meshes :ground])
        (three/create-wireframe [:background-meshes :grid]
                                (three/get-grid-vertices 12 1) [2 2 2] 2)
        (three/create-cube [:controllers :right] [0.1 0 0] [0 1 0 0] 0.05 :red)
        (three/create-cube [:controllers :left] [-0.1 0 0] [0 1 0 0] 0.05 :white)
        ;; (load-brick create-bricks)
        ;; (three/create-model [:background-meshes :room] "baked.glb" [0 0 0] [0 1 0 180] 1)
        ;; (assoc-in [:velocity] 0.0)
        ;; (assoc-in [:turn-velocity] 0.0)

        (three/create-cube [:meshes :cube] [2 4 0] [0 0 1 -20] [0.5 1 0.5] :yellow)
        (physics/create-cube [:meshes :cube] 5)
        (three/create-cube [:meshes :cube2] [2.2 6 0] [0 0 1 0] 0.5 :red)
        (physics/create-cube [:meshes :cube2] 1)
        )))

(defn set-velocity [world]
  (let [v (get-in world [:controllers :right :axes 1])
        f 0.02]
    (if (> (util/abs v) 0.1)
      (update-in world [:velocity] #(util/within (+ % (* v f -1)) -1 1))
      (update-in world [:velocity] #(* % 0.9)))))

(defn move [world]
  (let [camera-direction (-> (three/get-camera-direction world)
                             (assoc-in [1] 0))]
    (-> world
        (update-in [:camera :position] #(->> (:velocity world)
                                             (* 0.015)
                                             (vector/multiply camera-direction)
                                             (vector/add %)))
        (update-in [:camera] three/sync-object))))

(defn update-world [world]
  (-> world
      ;; controllers/update
      ;; (manipulation/move :right)
      ;; (manipulation/move :left)
      ;; set-velocity
      ;; move
      physics/step
      (update-in [:meshes :cube] physics/body-sync)
      (update-in [:meshes :cube2] physics/body-sync)
      ))

(defn create-and-grab [world hand]
  (if (nil? (get-in world [:picked-object hand]))
    (let [{:keys [position rotation]} (get-in world [:controllers hand])
          name (util/gen-keyword :brick)
          color (rand-nth (keys (get-in world [:brick :materials])))]
      (three/pulse world hand 1 50)
      (-> world
          (create-new-brick name position rotation color)
          (assoc-in [:picked-object hand] {:position [0 0 0]
                                           :rotation [1 0 0 0]
                                           :name name})))
    world))

(defn delete-brick [world hand]
  (let [scene (:scene world)
        brick-name (get-in world [:picked-object hand :name])
        brick (get-in world [:meshes brick-name])]
    (.remove scene (:object brick))
    (-> world
        (dissoc-in [:meshes brick-name])
        (dissoc-in [:picked-object hand]))))

(defn button-pressed [world {:keys [hand button]}]
  (cond
    (= button :grip) (manipulation/grab world hand)
    (= button :a) (create-and-grab world hand)
    (= button :b) (delete-brick world hand)
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
