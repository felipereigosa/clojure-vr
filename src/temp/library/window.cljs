(ns temp.library.window
  (:require [temp.library.util :as util]
            [temp.library.world :as world]
            [temp.library.transform :as transform]
            [temp.library.three :as three]
            [temp.core :as core]))

(def THREE js/window.THREE)

;; (def button-names [:trigger :grip nil nil :a :b nil])

;; (defonce button-states (atom {:left (vec (repeat 7 false))
;;                               :right (vec (repeat 7 false))}))

;; (defn update-buttons [world hand gamepad]
;;   (let [buttons (.-buttons gamepad)]
;;     (reduce (fn [w n]
;;               (let [button (nth buttons n)]
;;                 (if (not= (.-pressed button) (get-in @button-states [hand n]))
;;                   (let [event {:hand hand
;;                                :button (get button-names n)}]
;;                     (swap! button-states #(assoc-in % [hand n] (.-pressed button)))
;;                     (if (.-pressed button)
;;                       (core/button-pressed w event)
;;                       (core/button-released w event)))
;;                   w)))
;;             world
;;             (range (.-length buttons)))))

;; (defn update-controllers-state [world]
;;   (let [renderer (:renderer world)]
;;     (if-let [session (.getSession (.-xr renderer))]
;;       (reduce (fn [w input-source]
;;                 (let [gamepad (.-gamepad input-source)
;;                       hand (keyword (.-handedness input-source))
;;                       [_ _ x y] (array-seq (.-axes gamepad))]
;;                   (-> w
;;                       (update-buttons hand gamepad)
;;                       (assoc-in [:controllers hand :axes] [x y])
;;                       (assoc-in [:controllers hand :actuator]
;;                                 (first (.-hapticActuators gamepad))))))
;;               world (array-seq (.-inputSources session)))
;;       world)))

;; (defn update-controller-transform [world index name]
;;   (let [renderer (:renderer world)
;;         controller (.getController (.-xr renderer) index)
;;         position (.-position controller)
;;         position [(.-x position) (.-y position) (.-z position)]
;;         rotation (transform/quat->aa (.-quaternion controller))]
;;     (-> world
;;         (update-in [:controllers name]
;;                    #(merge % (transform/combine (:camera world)
;;                                                 {:position position
;;                                                  :rotation rotation})))
;;         (update-in [:controllers name] three/sync-object))))

;; (defn update-controllers [world]
;;   (-> world
;;       (update-controller-transform 0 :left)
;;       (update-controller-transform 1 :right)
;;       (update-controllers-state)))

(defn loop! []
  (let [{:keys [renderer scene camera]} @world/world]
    (swap! world/world core/update-world)
    (.render renderer scene (:camera camera))))

(defn init []
  (let [scene (new THREE.Scene)
        width (.-innerWidth js/window)
        height (.-innerHeight js/window)
        ratio (/ width height)
        camera (new THREE.PerspectiveCamera 30 ratio 0.1 100)
        renderer (new THREE.WebGLRenderer (clj->js {:antialias true}))
        holder (new THREE.Group)]
    (.set (.-position camera) 0 2 4)
    (.lookAt camera 0 0 0)
    (set! (.-background scene) (new THREE.Color 0x505050))

    (.setPixelRatio renderer (.-devicePixelRatio js/window))
    (.setSize renderer width height)
    (set! (.-outputEncoding renderer) THREE.sRGBEncoding)
    (set! (.-enabled (.-xr renderer)) true)
    (.setReferenceSpaceType (.-xr renderer) "local")
    (reset! world/world {:scene scene
                         :camera {:position [0 0 0]
                                  :rotation [1 0 0 0]
                                  :object holder
                                  :camera camera}
                         :renderer renderer})
    (swap! world/world core/create-world)
    (swap! world/world assoc-in [:button-functions] [#(core/button-pressed %1 %2)
                                                     #(core/button-released %1 %2)])
    (.setAnimationLoop renderer loop!)
    (.appendChild (.-body js/document) (.-domElement renderer))
    (.appendChild (.-body js/document) (.createButton js/window.VRButton renderer))
    (.add holder camera)
    (.add scene holder)
    (new js/window.OrbitControls camera renderer.domElement)
    1))

(defonce _ (init))

;; (when @world/reload?
;;   (println "reloaded")
;;   (let [scene (:scene @world/world)
;;         holder (new THREE.Group)]
;;     (.clear scene)
;;     (.add holder (:camera world/world))
;;     (.add scene holder)
;;     (reset! world/world
;;             (-> @world/world
;;                 (select-keys [:renderer :scene :camera])
;;                 (assoc-in [:holder] holder)))
;;     (swap! world/world core/create-world)))
