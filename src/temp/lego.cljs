(ns temp.lego
  (:require [temp.library.world :as world]
            [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.physics :as physics]))

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
        (physics/create-cube [:meshes :yellow-brick] 1 [0.618274 0.185482 0.309138]))))

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
