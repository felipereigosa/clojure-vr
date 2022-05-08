(ns temp.core
  (:require [temp.library.camera :as camera]
            [temp.library.vector :as vector]
            [temp.library.meshes :as mesh]
            [temp.library.canvas :as canvas]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.world :as world]
            [temp.library.matrix :as matrix]
            [temp.library.transform :as transform]))

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

      (assoc-in [:controller-right]
                (mesh/create-model "cube.obj" [0 0 0] [0 1 0 0] 0.05 :red))

      (assoc-in [:controller-left]
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
                (merge (mesh/create-model "cone.obj" [0.4 (+ 0.5 0.025 0.05) 0]
                                          [0 1 0 30] 0.1 :yellow)
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
                (merge (mesh/create-model "sphere.obj" [0 (+ 0.5 0.025 0.1) -0.2]
                                          [1 0 0 0] 0.1 :orange)
                       {:snaps [{:position [0 0 0]
                                 :rotation [1 0 0 0]
                                 :type :point}]}))

      ;;----------------------------------------------------------------------;;

      (assoc-in [:axis]
                (mesh/create-model "axis.obj" [0 0 0] [1 0 0 0] 1 nil))

      (assoc-in [:connections] [])
      (assoc-in [:pre-connection] nil)
      ))
(reset! world/reload? true)

(defn update-world [world]
  world)

(declare move-connected)

(defn draw-ghost-object [world]
  (if-let [[a b _ :as connection] (:pre-connection world)]
    (let [world (move-connected (update-in world [:connections] #(conj % connection)) a)]
      (mesh/draw world (-> (get-in world [:meshes b])
                           (assoc-in [:color] '(1 0 1)) ;;##################
                           )))))

(defn draw-axis [world mesh]
  (doseq [snap (:snaps mesh)]
    (mesh/draw world (merge (:axis world)
                            (matrix/combine-transforms snap mesh)))))

(defn draw-world! [world]
  (doseq [mesh (vals (:background-meshes world))]
    (mesh/draw world mesh))

  (doseq [mesh (vals (:meshes world))]
    (mesh/draw world mesh))

  (when (nil? (get-in world [:picked-object :left]))
    (mesh/draw world (:controller-left world)))
  (when (nil? (get-in world [:picked-object :right]))
    (mesh/draw world (:controller-right world)))
  (draw-ghost-object world)
  )

(defn get-closest-object [meshes position]
  (->> meshes
       (map (fn [[name mesh]]
              [name (vector/distance (:position mesh) position)]))
       (sort-by second)
       (filter #(< (second %) 0.1))
       first
       first))

(defn remove-connection [world]
  (let [left-object-name (get-in world [:picked-object :left :name])
        right-object-name (get-in world [:picked-object :right :name])]
    (if (and left-object-name right-object-name)
      (update-in world [:connections]
                 (fn [connections]
                   (vec (filter #(not (and (util/in? left-object-name %)
                                           (util/in? right-object-name %)))
                                connections))))
      world)))

(defn button-pressed [world {:keys [hand button]}]
  (if (= button :grip)
    (let [actuator (get-in world [:actuators hand])
          controller (get-in world [(util/join-keywords :controller hand)])]
      (if-let [object-name (get-closest-object (:meshes world) (:position controller))]
        (let [object (get-in world [:meshes object-name])
              relative-transform (matrix/remove-transform object controller)]
          (.pulse actuator 1 50)
          (-> world
              (assoc-in [:picked-object hand] (assoc relative-transform :name object-name))
              remove-connection))
        world))
    world))

(defn button-released [world {:keys [hand button]}]
  (let [world (if (and (= button :grip)
                       (= hand :right))
                (if-let [connection (:pre-connection world)]
                  (-> world
                      (dissoc-in [:pre-connection])
                      (update-in [:connections] #(conj % connection)))
                  world)
                world)]
    (if (= button :grip)
      (dissoc-in world [:picked-object hand])
      world)))

(defn prepare-connection [[a b transform] root-name]
  (if (= a root-name)
    [b transform]
    [a (transform/invert transform)]))

(defn move-connected [world root-name]
  (let [connections (filter #(util/in? root-name %) (:connections world))
        root-object (get-in world [:meshes root-name])]
    (reduce (fn [w connection]
              (let [[other-name relative-transform] (prepare-connection connection root-name)
                    transform (matrix/combine-transforms relative-transform root-object)]
                (-> w
                    (update-in [:meshes other-name] #(merge % transform))
                    ;; (move-connected w other-name) #######################################
                    )))
            world
            connections)))

(defn move-picked-object [world hand]
  (if-let [picked-object (get-in world [:picked-object hand])]
    (let [controller (get-in world [(util/join-keywords :controller hand)])
          name (:name picked-object)
          transform (matrix/combine-transforms picked-object controller)]
      (-> world
          (update-in [:meshes name] #(merge % transform))
          (move-connected name)))
    world))

(defn snaps-match? [left-object left-snap right-object right-snap]
  (let [global-left-snap (matrix/combine-transforms left-snap left-object)
        global-right-snap (matrix/combine-transforms right-snap right-object)
        distance (vector/distance (:position global-left-snap)
                                  (:position global-right-snap))
        x-axis [1 0 0]
        y-axis [0 1 0]
        left-rotation (merge global-left-snap {:position [0 0 0]})
        x-left (matrix/apply-transform left-rotation x-axis)
        y-left (matrix/apply-transform left-rotation y-axis)
        right-rotation (merge global-right-snap {:position [0 0 0]})
        x-right (matrix/apply-transform right-rotation x-axis)
        y-right (matrix/apply-transform right-rotation y-axis)
        type (:type right-snap)]
    (if (and (< distance 0.1)
             (or (= type :point)
                 (and
                   (< (vector/dot-product y-left y-right) -0.8)
                   (or (= type :direction)
                       (> (vector/dot-product x-left x-right) 0.8)))))
      distance
      nil)))

(defn get-object-relative-transform [target-snap source-snap]
  (let [x-rotation {:position [0 0 0]
                  :rotation [1 0 0 180]}]
    (matrix/combine-transforms
      (transform/invert
        (matrix/combine-transforms x-rotation source-snap)) target-snap)))

(defn get-snap-transform [left-object right-object]
  (let [snaps (mapcat (fn [left-snap]
                        (map (fn [right-snap]
                               [(snaps-match? left-object left-snap
                                              right-object right-snap)
                                left-snap right-snap])
                             (:snaps right-object)))
                      (:snaps left-object))]
    (if-let [[_ left-snap right-snap] (->> snaps
                                           (filter (comp not nil? first))
                                           (sort-by first)
                                           first)]
      (get-object-relative-transform left-snap right-snap)
      nil)))

(defn snap-object [world]
  (let [left-object-name (get-in world [:picked-object :left :name])
        right-object-name (get-in world [:picked-object :right :name])]
    (if (and left-object-name
             right-object-name)
      (let [left-object (get-in world [:meshes left-object-name])
            right-object (get-in world [:meshes right-object-name])]
        (if-let [snap-transform (get-snap-transform left-object right-object)]
          (if (nil? (:pre-connection world))
            (do
              (.pulse (get-in world [:actuators :left]) 1 100)
              (.pulse (get-in world [:actuators :right]) 1 100)
              (assoc-in world [:pre-connection] [left-object-name
                                                 right-object-name
                                                 snap-transform]))
            world)
          (dissoc-in world [:pre-connection])))
      world)))

(defn controller-moved [world {:keys [position rotation hand] :as event}]
  (-> world
      (update-in [(util/join-keywords :controller hand)]
                 #(merge %
                         {:position (vector/add position [0 1 0.7])
                          :rotation rotation}))
      (move-picked-object :right)
      (move-picked-object :left)
      (snap-object)
      ))
