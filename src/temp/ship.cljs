(ns temp.ship
  (:require [temp.library.world :as world]
            [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.vector :as vector]
            [temp.library.transform :as transform]
            [temp.library.controllers :as controllers]
            [temp.navigation :as navigation]))

(defn assign-meshes [world]
  (let [ground (three/get-object world "ground")
        ship (three/get-object world "ship")
        sea (three/get-object world "sea")]
    (set! (.-visible ground) false)
    (-> world
        (three/assign-mesh [:meshes :ship] ship)
        (update-in [:meshes :ship]
                   #(-> %
                        (assoc-in [:rotation] [1 0 0 30])
                        three/sync-object))
        (three/assign-mesh [:meshes :sea] sea)
        (assoc-in [:loaded] true))))

(defn create-world [world]
  (let []
    (-> world
        (three/set-clear-color [0 0 0])
        (three/create-lights [-1 1 -1])
        (assoc-in [:camera :position] [-0.25 1.37 -0.85])
        (assoc-in [:camera :rotation] [0 1 0 0])
        (update-in [:camera] three/sync-object)
        (three/create-cube [:controllers :right] [0.1 0 0] [0 1 0 0] 0.05 :red)
        (three/create-cube [:controllers :left] [-0.1 0 0] [0 1 0 0] 0.05 :white)
        (three/create-model [:background-meshes :room] "ship.glb"
                            [0 0 0] [0 1 0 0] 1 assign-meshes)
        (assoc-in [:t] 0.0))))

(defn update-world [world]
  (if (:loaded world)
    (-> world
        controllers/update
        navigation/move
        (update-in [:t] #(+ % 0.5))
        (update-in [:meshes :ship]
                   #(-> %
                        (assoc-in [:rotation 3] (* 5 (util/sin (:t world))))
                        three/sync-object))

        (update-in [:meshes :sea]
                   #(-> %
                        (assoc-in [:position 0] (mod (* -0.01 (:t world)) 25))
                        three/sync-object))
        )
    world))

;; (defn grab [world hand]
;;   (let [scene (get-in world [:background-meshes :room :object])
;;         point (get-in world [:controllers hand :position])
;;         box (three/get-object scene "handle-collision")]
;;     (if-let [object-name (three/inside-object? box point)]
;;       (assoc-in world [:picked-object hand :name] object-name)
;;       world)))

(defn button-pressed [world {:keys [hand button]}]
  ;; (cond
  ;;   (= button :grip) (grab world hand)
  ;;   :else world)
  world
  )

(defn button-released [world {:keys [hand button]}]
  ;; (if (= button :grip)
  ;;   (dissoc-in world [:picked-object hand])
  ;;   world)
  world
  )
