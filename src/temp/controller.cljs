(ns temp.controller
  (:require [temp.library.mesh :as mesh]
            [temp.library.transform :as transform]))

(defn set-transform [world hand position rotation]
  (-> world
      (assoc-in [:controller hand :position] position)
      (assoc-in [:controller hand :rotation] rotation)))

(defn get-transform [world hand]
  (let [controller (get-in world [:controller hand])]
    (transform/combine controller (:camera world))))

(defn draw! [world hand]
  (let [controller (get-in world [:controller hand])
        transform (get-transform world hand)]
    (mesh/draw world (merge controller transform))))

(defn pulse [world hand strength duration]
  (let [actuator (get-in world [:actuators hand])]
    (.pulse actuator strength duration)))
