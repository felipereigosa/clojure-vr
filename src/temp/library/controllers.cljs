(ns temp.library.controllers
  (:refer-clojure :exclude [update])
  (:require [temp.library.transform :as transform]
            [temp.library.three :as three]
            [temp.library.world :as world]))

(def button-names [:trigger :grip nil nil :a :b nil])

(defonce button-states (atom {:left (vec (repeat 7 false))
                              :right (vec (repeat 7 false))}))

(defn set-index! [renderer index]
  (.addEventListener (.getController (.-xr renderer) index)
                     "connected"
                     (fn [event]
                       (let [hand (keyword (.. event -data -handedness))]
                         (swap! world/world #(assoc-in % [:controllers hand :index] index))))))

(defn update-buttons [world hand gamepad]
  (let [buttons (.-buttons gamepad)
        [button-pressed button-released] (:button-functions world)]
    (reduce (fn [w n]
              (let [button (nth buttons n)]
                (if (not= (.-pressed button) (get-in @button-states [hand n]))
                  (let [event {:hand hand
                               :button (get button-names n)}]
                    (swap! button-states #(assoc-in % [hand n] (.-pressed button)))
                    (if (.-pressed button)
                      (button-pressed w event)
                      (button-released w event)))
                  w)))
            world
            (range (.-length buttons)))))

(defn update-state [world]
  (let [renderer (:renderer world)]
    (if-let [session (.getSession (.-xr renderer))]
      (reduce (fn [w input-source]
                (let [gamepad (.-gamepad input-source)
                      hand (keyword (.-handedness input-source))
                      [_ _ x y] (array-seq (.-axes gamepad))]

                  (-> w
                      (update-buttons hand gamepad)
                      (assoc-in [:controllers hand :axes] [x y])
                      (assoc-in [:controllers hand :actuator]
                                (first (.-hapticActuators gamepad))))))
              world (array-seq (.-inputSources session)))
      world)))

(defn update-transform [world index]
  (let [renderer (:renderer world)
        controller (.getController (.-xr renderer) index)
        position (.-position controller)
        position [(.-x position) (.-y position) (.-z position)]
        rotation (transform/quat->aa (.-quaternion controller))
        name (if (= (get-in world [:controllers :right :index]) index)
               :right
               :left)]
    (-> world
        (update-in [:controllers name]
                   #(merge % (transform/combine (:camera world)
                                                {:position position
                                                 :rotation rotation})))
        (update-in [:controllers name] three/sync-object))))

(defn update [world]
  (-> world
      (update-transform 0)
      (update-transform 1)
      (update-state)))
