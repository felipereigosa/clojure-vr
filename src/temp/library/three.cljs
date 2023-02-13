(ns temp.library.three
  (:refer-clojure :exclude [clone])
  (:require [temp.library.world :as world]
            [temp.library.util :as util]
            [temp.library.vector :as vector]
            [temp.library.transform :as transform]))

(def THREE js/window.THREE)

(defn point->vector [point]
  [(.-x point) (.-y point) (.-z point)])

(defn get-color [color]
  (if (keyword? color)
    (new THREE.Color (name color))
    (let [[r g b] color]
      (new THREE.Color (/ r 255) (/ g 255) (/ b 255)))))

(defn sync-object [obj]
  (let [{:keys [position rotation scale color object]} obj]
    (if (not (nil? color))
      (set! (.-color (.-material object)) (get-color color)))

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

(defn set-visible [mesh value]
  (set! (.-visible (:object mesh)) value)
  mesh)

(defn get-object [world name]
  (.getObjectByName (:scene world) name true))

(defn assign-mesh [world path object]
  (let [position (point->vector (.-position object))]
    (assoc-in world path (sync-object
                           {:object object
                            :position position
                            :rotation [0 1 0 0]}))))

(defn create-mesh [world path geometry position rotation scale color]
  (let [material (new THREE.MeshPhongMaterial
                      #js{:color (get-color color)})
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
                      #js{:color (get-color color)
                          :linewidth line-width})
        points (apply array (map (fn [[x y z]]
                                   (new THREE.Vector3 x y z))
                                 vertices))
        geometry (new THREE.BufferGeometry)]
    (.setFromPoints geometry points)
    (let [mesh (new THREE.LineSegments geometry material)]
      (.add (:scene world) mesh)
      (assoc-in world path {:object mesh
                            :color color}))))

(defn create-model [w path filename position rotation scale & [loaded]]
  (let [loader (new js/window.GLTFLoader)
        w (update-in w [:to-load] inc)]
    (.load loader filename (fn [gltf]
                             (let [scene (.-scene gltf)
                                   has-animation? (not (empty? (.-animations gltf)))]
                               (.add (:scene @world/world) scene)
                               (swap! world/world
                                      #(cond-> %
                                         true (assoc-in path (sync-object
                                                               {:object scene
                                                                :position position
                                                                :rotation rotation
                                                                :scale scale}))
                                         has-animation? (assoc-in (conj path :clips) (.-animations gltf))
                                         has-animation? (assoc-in (conj path :mixer)
                                                                  (new THREE.AnimationMixer scene))
                                         loaded loaded
                                         true (update-in [:to-load] dec))))))
    w))

(defn direction->rotation [direction]
  (let [direction (vector/normalize direction)]
    (cond
      (vector/equal? direction [0 1 0]) [0 1 0 0]
      (vector/equal? direction [0 -1 0]) [1 0 0 180]
      :else (let [axis (vector/cross-product [0 1 0] direction)
                  angle (vector/angle direction [0 1 0])]
              (conj (vector/normalize axis) angle)))))

(defn create-segment [world start end]
  (let [n (count (:segments world))
        v (vector/subtract end start)
        rotation (direction->rotation v)
        position (vector/multiply (vector/add start end) 0.5)
        length (vector/length v)]
    (create-cylinder world [:segments n] position rotation
                     [0.12 length 0.12] :white)))

(defn set-clear-color [world color]
  (set! (.-background (:scene world)) (get-color color))
  world)

(defn create-lights [world [x y z] shadow?]
  (let [hemisphere-light (new THREE.HemisphereLight 0x606060 0x404040)
        directional-light (new THREE.DirectionalLight 0xaaaaaa)
        scene (:scene world)
        camera (:camera world)
        d 40]
    (when shadow?
      (set! directional-light.castShadow true)
      (set! directional-light.shadow.mapSize.width 4096)
      (set! directional-light.shadow.mapSize.height 4096)

      ;; (set! directional-light.shadow.camera.left (- d))
      ;; (set! directional-light.shadow.camera.right d)
      ;; (set! directional-light.shadow.camera.top d)
      ;; (set! directional-light.shadow.camera.bottom (- d))
      ;; (set! directional-light.shadow.camera.far (* 3 d))
      ;; (set! directional-light.shadow.camera.near d)
      )

    ;; (.add scene (new THREE.DirectionalLightHelper directional-light 5))

    (.set (.-position directional-light) x y z)
    (.add (:object camera) hemisphere-light)
    (.add (:object camera) directional-light)
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

(defn clone-mesh [world path mesh]
  (let [new-object (.clone (:object mesh))
        new-mesh (assoc-in mesh [:object] new-object)]
    (.add (:scene world) new-object)
    (assoc-in world path new-mesh)))


(defn inside-object? [object [px py pz]]
  (let [raycaster (new THREE.Raycaster)
        point (new THREE.Vector3 px py pz)]
    (.updateMatrixWorld object)
    (.set raycaster point (new THREE.Vector3 1 0 0))
    (set! (.. object -material -side) 2)
    (= (mod (count (.intersectObject raycaster object)) 2) 1)))

(defn play-action [object action-name & [time-scale]]
  (let [mixer (:mixer object)
        clip (.findByName THREE.AnimationClip (:clips object) (util/dekeyword action-name))
        action (.clipAction mixer clip)]
    (.stopAllAction mixer)
    (.reset action)
    (when time-scale
      (set! (.-timeScale action) time-scale))
    (set! (.-loop action) THREE.LoopOnce)
    (set! (.-clampWhenFinished action) true)
    (.play action)))




