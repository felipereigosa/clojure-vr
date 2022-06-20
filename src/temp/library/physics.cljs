(ns temp.library.physics
  (:require [temp.library.util :as util]
            [temp.library.three :as three]
            [temp.library.transform :as transform]
            [temp.library.vector :as vector]))

(def CANNON js/window.CANNON)

(defn create-planet [gravity]
  (let [planet (new CANNON.World)]
    (.set (.-gravity planet) 0 (- gravity) 0)
    planet))

(defn create-floor [world path]
  (let [planet (:planet world)
        shape (new CANNON.Plane)
        body (new CANNON.Body #js{:mass 0})]
    (.addShape body shape)
    (.setFromEuler (.-quaternion body) -1.57 0 0)
    (.addBody planet body)
    (assoc-in world (conj path :body) body)))

(defn create-cube [world path mass]
  (let [planet (:planet world)
        mesh (get-in world path)
        [x y z] (:position mesh)
        [rx ry rz angle] (:rotation mesh)
        scale (:scale mesh)
        [sx sy sz] (vector/multiply
                     (if (vector? scale)
                       scale
                       (vec (repeat 3 scale))) 0.5)
        shape (new CANNON.Box (new CANNON.Vec3 sx sy sz))
        body (new CANNON.Body #js{:mass mass})]
    (.addShape body shape)
    (.set (.-position body) x y z)
    (.setFromAxisAngle (.-quaternion body)
                       (new THREE.Vector3 rx ry rz)
                       (util/to-radians angle))
    (.addBody planet body)
    (assoc-in world (conj path :body) body)))

(defn body-sync [mesh]
  (let [p (.-position (:body mesh))
        q (.-quaternion (:body mesh))]
    (-> mesh
        (assoc-in [:position] [(.-x p) (.-y p) (.-z p)])
        (assoc-in [:rotation] (transform/quat->aa q))
        three/sync-object)))

(defn step [world]
  (.fixedStep (:planet world))
  world)
