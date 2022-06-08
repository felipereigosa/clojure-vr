(ns temp.library.three
  (:require [temp.library.world :as world]
            [temp.library.util :as util]
            [temp.library.vector :as vector]
            [temp.library.transform :as transform]))

(def THREE js/window.THREE)

(defn sync-object [obj]
  (let [{:keys [position rotation scale object]} obj]
    (if-let [[x y z] position]
      (.set (.-position object) x y z))

    (if-let [[rx ry rz angle] rotation]
      (.setFromAxisAngle (.-quaternion object)
                         (new THREE.Vector3 rx ry rz)
                         (util/to-radians angle)))
    (if scale
      (let [[sx sy sz] (if (vector? scale)
                         scale
                         (vec (repeat 3 scale)))]
        (.set (.-scale object) sx sy sz)))
    obj))

(defn get-color [color]
  (if (keyword? color)
    (new THREE.Color (name color))
    (let [[r g b] color]
      (new THREE.Color (/ r 255) (/ g 255) (/ b 255)))))

(defn create-mesh [world path geometry position rotation scale color]
  (let [material (new THREE.MeshPhongMaterial
                      (clj->js {:color (get-color color)}))
        mesh (new THREE.Mesh geometry material)]
    (.add (:scene world) mesh)
    (assoc-in world path (sync-object
                           {:object mesh
                            :position position
                            :rotation rotation
                            :scale scale
                            :color color}))))

(defn create-cube [world path position rotation scale color]
  (create-mesh world path
               (new THREE.BoxGeometry)
               position rotation scale color))

(defn create-cone [world path position rotation scale color]
  (create-mesh world path
               (new THREE.ConeGeometry 0.5 1 32)
               position rotation scale color))

(defn create-cylinder [world path position rotation scale color]
  (create-mesh world path
               (new THREE.CylinderGeometry 0.5 0.5 1 32)
               position rotation scale color))

(defn create-sphere [world path position rotation scale color]
  (create-mesh world path
               (new THREE.SphereGeometry 0.5 32 16)
               position rotation scale color))

(defn create-wireframe [world path vertices color line-width]
  (let [material (new THREE.LineBasicMaterial
                      (clj->js {:color (get-color color)
                                :linewidth line-width}))
        points (apply array (map (fn [[x y z]]
                                   (new THREE.Vector3 x y z))
                                 vertices))
        geometry (new THREE.BufferGeometry)]
    (.setFromPoints geometry points)
    (let [mesh (new THREE.LineSegments geometry material)]
      (.add (:scene world) mesh)
      (assoc-in world path {:object mesh
                            :color color}))))

(defn create-model [w path filename position rotation scale]
  (let [loader (new js/window.GLTFLoader)
        object (atom nil)]
    (.load loader filename (fn [gltf]
                             (let [scene (.-scene gltf)]
                               (.add (:scene @world/world) scene)
                               (swap! world/world
                                      assoc-in path (sync-object
                                                      {:object scene
                                                       :position position
                                                       :rotation rotation
                                                       :scale scale})))))
    w))

(defn set-clear-color [world color]
  (set! (.-background (:scene world)) (get-color color))
  world)

(defn create-lights [world]
  (let [hemisphere-light (new THREE.HemisphereLight 0x606060 0x404040)
        directional-light (new THREE.DirectionalLight 0xffffff)
        scene (:scene world)]
    (.set (.-position directional-light) 1 1 1)
    (.add scene hemisphere-light)
    (.add scene directional-light)
    world))

(defn get-grid-vertices [num-cells cell-size]
  (let [hw (/ (* cell-size num-cells) 2)
        seq (map (fn [val]
                   (- (* val cell-size) hw))
                 (range (inc num-cells)))
        min (first seq)
        max (last seq)
        z-parallel (mapcat (fn [x]
                             [x 0 min x 0 max])
                           seq)
        x-parallel (mapcat (fn [z]
                             [min 0 z max 0 z])
                           seq)]
    (partition 3 (concat z-parallel x-parallel))))

(defn pulse [world hand strength duration]
  (let [actuator (get-in world [:controllers hand :actuator])]
    (.pulse actuator strength duration)
    world))
