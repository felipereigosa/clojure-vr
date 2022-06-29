(ns temp.manipulation
  (:require [temp.library.transform :as transform]
            [temp.library.vector :as vector]
            [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]))

(defn get-brick-at [world [px py pz]]
  (let [raycaster (new THREE.Raycaster)]
    (.set raycaster (new THREE.Vector3 px py pz) (new THREE.Vector3 1 1 1))
    (first (util/find-if (fn [[brick-name brick]]
                           (let [collision-cube (-> (:collision-cube world)
                                                    (assoc-in [:scale] [0.618274 0.185482 0.309138]) ;;##########
                                                    (assoc-in [:rotation] (:rotation brick))
                                                    (assoc-in [:position] (:position brick))
                                                    three/sync-object)]
                             (= (mod (count (.intersectObject raycaster (:object collision-cube))) 2) 1)))
                         (:meshes world)))))

(defn grab [world hand]
  (let [controller-transform (get-in world [:controllers hand])]
    (if-let [object-name (get-brick-at world (:position controller-transform))]
      (let [object (get-in world [:meshes object-name])
            relative-transform (transform/remove controller-transform object)]
        (three/pulse world hand 1 50)
        (set! (.-type (:body object)) CANNON.Body.KINEMATIC)
        (assoc-in world [:picked-object hand] (assoc relative-transform :name object-name)))
      world)))

(defn move [world hand]
  (if-let [{:keys [name] :as picked-object} (get-in world [:picked-object hand])]
    (let [transform (transform/combine (get-in world [:controllers hand]) picked-object)]
      (-> world
          (update-in [:meshes name] #(merge % transform))
          (update-in [:meshes name] three/sync-object)))
    world))

(defn snaps-match? [object-b snap-b object-a snap-a]
  (let [global-snap-b (transform/combine object-b snap-b)
        global-snap-a (transform/combine object-a snap-a)
        distance (vector/distance (:position global-snap-b)
                                  (:position global-snap-a))
        x-axis [1 0 0]
        y-axis [0 1 0]
        rotation-b (merge global-snap-b {:position [0 0 0]})
        x-left (transform/apply rotation-b x-axis)
        y-left (transform/apply rotation-b y-axis)
        rotation-a (merge global-snap-a {:position [0 0 0]})
        x-right (transform/apply rotation-a x-axis)
        y-right (transform/apply rotation-a y-axis)
        type (:type snap-a)]
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
    (transform/combine
      target-snap
      (transform/invert
        (transform/combine source-snap x-rotation)))))

(defn get-snap-transform [world object-name]
  (let [object-a (get-in world [:meshes object-name])
        snaps (mapcat (fn [target-name]
                        (if (not (= target-name object-name))
                          (let [object-b (get-in world [:meshes target-name])]
                            (mapcat (fn [snap-b]
                                   (map (fn [snap-a]
                                          [(snaps-match? object-b snap-b
                                                         object-a snap-a)
                                           snap-b snap-a object-b])
                                        (:snaps object-a)))
                                 (:snaps object-b)))))
                      (keys (:meshes world)))]
    (if-let [[_ snap-b snap-a object-b]
             (->> snaps
                  (filter (comp not nil? first))
                  (sort-by first)
                  first)]
      (transform/combine object-b (get-object-relative-transform snap-b snap-a))
      nil)))

(defn snap [world hand]
  (if-let [object-name (get-in world [:picked-object hand :name])]
    (if-let [snap-transform (get-snap-transform world object-name)]
      (do
        (three/pulse world hand 1 100)
        (update-in world [:meshes object-name]
                   #(-> %
                        (merge snap-transform)
                        (three/sync-object))))
      world)
    world))

(defn release [world hand]
  (if-let [object-name (get-in world [:picked-object hand :name])]
    (let [body (get-in world [:meshes object-name :body])]
      (set! (.-type body) CANNON.Body.DYNAMIC)
      (-> world
          (snap hand)
          (dissoc-in [:picked-object hand])))
    world))
