(ns temp.manipulation
  (:require [temp.library.transform :as transform]
            [temp.library.vector :as vector]
            [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]))

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

(defn grab [world hand]
  (let [controller-transform (get-in world [:controllers hand])]
    (if-let [object-name (get-closest-object (:meshes world)
                                             (:position controller-transform))]
      (let [object (get-in world [:meshes object-name])
            relative-transform (transform/remove controller-transform object)]
        (three/pulse world hand 1 50)
        (-> world
            (assoc-in [:picked-object hand] (assoc relative-transform :name object-name))
            remove-connection))
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
                    transform (transform/combine root-object relative-transform)]
                (-> w
                    (update-in [:meshes other-name] #(merge % transform))
                    (update-in [:meshes other-name] three/sync-object)
                    (move-connected w other-name))))
            world
            connections)))

(defn move [world hand]
  (if-let [{:keys [name] :as picked-object} (get-in world [:picked-object hand])]
    (let [transform (transform/combine (get-in world [:controllers hand]) picked-object)]
      (-> world
          (update-in [:meshes name] #(merge % transform))
          (update-in [:meshes name] three/sync-object)
          (move-connected name)))
    world))

(defn release [world hand]
  (let [world (if (= hand :right)
                (if-let [connection (:pre-connection world)]
                  (-> world
                      (dissoc-in [:pre-connection])
                      (update-in [:connections] #(conj % connection)))
                  world)
              world)]
    (dissoc-in world [:picked-object hand])))

(defn snaps-match? [left-object left-snap right-object right-snap]
  (let [global-left-snap (transform/combine left-object left-snap)
        global-right-snap (transform/combine right-object right-snap)
        distance (vector/distance (:position global-left-snap)
                                  (:position global-right-snap))
        x-axis [1 0 0]
        y-axis [0 1 0]
        left-rotation (merge global-left-snap {:position [0 0 0]})
        x-left (transform/apply left-rotation x-axis)
        y-left (transform/apply left-rotation y-axis)
        right-rotation (merge global-right-snap {:position [0 0 0]})
        x-right (transform/apply right-rotation x-axis)
        y-right (transform/apply right-rotation y-axis)
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
    (transform/combine
      target-snap
      (transform/invert
        (transform/combine source-snap x-rotation)))))

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
            (-> world
                (three/pulse :left 1 100)
                (three/pulse :right 1 100)
                (assoc-in [:pre-connection] [left-object-name
                                             right-object-name
                                             snap-transform]))
            world)
          (dissoc-in world [:pre-connection])))
      world)))
