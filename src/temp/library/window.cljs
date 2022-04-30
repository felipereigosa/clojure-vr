(ns temp.library.window
  (:require [temp.library.util :as util]
            [temp.library.vector :as vector]
            [temp.library.matrix :as matrix]
            [temp.library.world :as world]
            [temp.core :as core]))

(def time-since-update (atom 0))

(defn load-shader [gl type source]
  (let [shader (.createShader gl type)]
    (.shaderSource gl shader source)
    (.compileShader gl shader)
    (if (not (.getShaderParameter gl shader (.-COMPILE_STATUS gl)))
      (do
        (js/alert "Error loading shader")
        (println (.getShaderInfoLog gl shader))
        (.deleteShader gl shader))
      shader)))

(defn create-locations [gl program]
  {:projection-matrix (.getUniformLocation gl program "projectionMatrix")
   :view-matrix (.getUniformLocation gl program "viewMatrix")
   :model-matrix (.getUniformLocation gl program "modelMatrix")
   :camera-matrix (.getUniformLocation gl program "cameraMatrix")
   :material-color (.getUniformLocation gl program "materialColor")
   :color (.getAttribLocation gl program "color")
   :position (.getAttribLocation gl program "position")
   :normal (.getAttribLocation gl program "normal")})

(defn compile-program [gl name]
  (let [vs-source (get @world/data-map (str name "-vert.glsl"))
        vertex-shader (load-shader gl (.-VERTEX_SHADER gl) vs-source)
        fs-source (get @world/data-map (str name "-frag.glsl"))
        fragment-shader (load-shader gl (.-FRAGMENT_SHADER gl) fs-source)
        program (.createProgram gl)]
    (.attachShader gl program vertex-shader)
    (.attachShader gl program fragment-shader)
    (.linkProgram gl program)
    (.useProgram gl program)
    {:index program
     :locations (create-locations gl program)}))

;; (def button (atom nil))

;; (defn get-button-name [value]
;;   (get {1 :left
;;         4 :middle
;;         2 :right}
;;        value))

;; (defn create-listener [handler]
;;   (fn [event]
;;     (let [canvas (.-target event)
;;           rect (.getBoundingClientRect canvas)
;;           x (- (.-clientX event) (.-left rect))
;;           y (- (.-clientY event) (.-top rect))
;;           b (or (get-button-name (.-buttons event))
;;                 @button)
;;           e {:x x
;;              :y y
;;              :button b}]
;;       (reset! button b)
;;       (if (not (nil? @world/world))
;;         (swap! world/world handler e)))))

;; (defn create-listeners! [canvas]
;;   (let [canvas (util/get-by-id :canvas-2d)]

;;     (set! (.-onmousedown canvas)
;;       (create-listener (fn [w e]
;;                          (reset! time-since-update 0)
;;                          (core/mouse-pressed w e))))

;;     (set! (.-onmousemove canvas)
;;       (create-listener (fn [w e]
;;                          (reset! time-since-update 0)
;;                          (core/mouse-moved w e))))

;;     (set! (.-onmouseup canvas)
;;       (create-listener (fn [w e]
;;                          (reset! time-since-update 0)
;;                          (core/mouse-released w e))))

;;     (set! (.-onwheel canvas)
;;           (fn [event]
;;             (let [value (if (pos? (.-deltaY event)) -1 1)]
;;               (swap! world/world core/mouse-scrolled value))))

;;     (set! (.-oncontextmenu canvas)
;;           (fn [event] (.preventDefault event)))

;;     (set! (.-onkeydown js/window)
;;           (fn [event]
;;             (swap! world/world core/key-pressed
;;                    {:name (.-key event)
;;                     :code (.-code event)})))

;;     (set! (.-onkeyup js/window)
;;           (fn [event]
;;             (swap! world/world core/key-released
;;                    {:name (.-key event)
;;                     :code (.-code event)})))))

(defn quat->axis-angle [input]
  (let [[x y z w] input
        q (.fromValues gl-matrix/quat x y z w)
        axis (.create gl-matrix/vec3)
        angle (.getAxisAngle gl-matrix/quat axis q)]
    (conj (vector/normalize (js->clj axis))
          (util/to-degrees angle))))

(def button-names [:trigger :grip nil nil :a :b nil])

(defonce button-states (atom {:left (vec (repeat 7 false))
                              :right (vec (repeat 7 false))}))

(defn update-buttons [world hand gamepad]
  (let [buttons (.-buttons gamepad)]
    (reduce (fn [w n]
              (let [button (nth buttons n)]
                (if (not= (.-pressed button) (get-in @button-states [hand n]))
                  (let [event {:hand hand
                               :button (get button-names n)}]
                    (swap! button-states #(assoc-in % [hand n] (.-pressed button)))
                    (if (.-pressed button)
                      (core/button-pressed w event)
                      (core/button-released w event)))
                  w)))
            world
            (range (.-length buttons)))))

(defn update-input-sources [world frame]
  (let [ref-space (:ref-space world)
        session (.-session frame)]
    (reduce (fn [w input-source]
              (let [pose (.getPose frame (.-gripSpace input-source) ref-space)
                    transform (.-transform pose)
                    position (.-position transform)
                    rotation (.-orientation transform)
                    gamepad (.-gamepad input-source)
                    hand (keyword (.-handedness input-source))
                    w (update-buttons w hand gamepad)]
                (core/controller-moved w {:position [(.-x position) (.-y position) (.-z position)]
                                          :rotation (quat->axis-angle [(.-x rotation) (.-y rotation)
                                                                       (.-z rotation) (.-w rotation)])
                                          :hand hand
                                          :buttons (map (fn [button]
                                                          {:pressed (.-pressed button)
                                                           :value (.-value button)
                                                           :touched (.-touched button)})
                                                        (.-buttons gamepad))
                                          :axes (array-seq (.-axes gamepad))})))
            world (array-seq (.-inputSources session)))))

(defn loop! [time frame]
  (let [session (.-session frame)
        world @world/world
        gl (:gl world)]
    (.requestAnimationFrame session loop!)
    (when (< @time-since-update 5000)
      (swap! world/world core/update-world)
      (swap! world/world #(update-input-sources % frame))
      (let [pose (.getViewerPose frame (:ref-space @world/world))
            gl-layer (.-baseLayer (.-renderState session))]
        (.bindFramebuffer gl (.-FRAMEBUFFER gl) (.-framebuffer gl-layer))
        (.clear gl (bit-or (.-COLOR_BUFFER_BIT gl)
                           (.-DEPTH_BUFFER_BIT gl)))

        (doseq [view (.-views pose)]
          (let [viewport (.getViewport gl-layer view)]
            (.viewport gl (.-x viewport) (.-y viewport)
                       (.-width viewport) (.-height viewport))
            (-> @world/world
                (assoc-in [:projection-matrix] (.-projectionMatrix view))
                (assoc-in [:view-matrix]
                          (.-matrix (.-inverse (.-transform view))))
                (core/draw-world!))))

        (swap! time-since-update #(+ 16 %)) ;;######################## use time
        (when (core/keep-active? @world/world)
          (reset! time-since-update 0))))))

(defn init-world-gl [world gl ref-space]
  (.enable gl (.-CULL_FACE gl))
  (.cullFace gl (.-GL_BACK gl))
  (.clearDepth gl 1.0)
  (.enable gl (.-DEPTH_TEST gl))
  (.depthFunc gl (.-LEQUAL gl))
  (-> world
      (assoc-in [:gl] gl)
      (assoc-in [:ref-space] ref-space)
      (assoc-in [:programs :flat] (compile-program gl "flat"))
      ;; (assoc-in [:programs :colored] (compile-program gl "colored"))
      (assoc-in [:programs :wireframe] (compile-program gl "wireframe"))
      (assoc-in [:buffers] {:position (.createBuffer gl)
                            :normal (.createBuffer gl)
                            :color (.createBuffer gl)})))

(defn on-session-started [session]
  (let [canvas (.createElement js/document "canvas")
        canvas-2d (.createElement js/document "canvas")
        gl (.getContext canvas "webgl2" (clj->js {:antialias true :xrCompatible true}))]
    (.updateRenderState session (clj->js {:baseLayer (js/XRWebGLLayer. session gl)}))
    (-> (.requestReferenceSpace session "local")
        (.then (fn [rs]
                 (swap! world/world #(assoc-in % [:ctx] (.getContext canvas-2d "2d")))
                 (swap! world/world #(init-world-gl % gl rs))
                 (swap! world/world core/create-world)
                 ;; (create-listeners! canvas)
                 (.requestAnimationFrame session loop!))))))

(defn init []
  (let [filenames ["flat-vert.glsl"
                   "flat-frag.glsl"
                   ;; "colored-vert.glsl"
                   ;; "colored-frag.glsl"
                   "wireframe-vert.glsl"
                   "wireframe-frag.glsl"
                   "cube.obj"
                   "cube.mtl"
                   "sphere.obj"
                   "sphere.mtl"
                   "cylinder.obj"
                   "cylinder.mtl"
                   "cone.obj"
                   "cone.mtl"
                   ;; "red-yellow.obj"
                   ;; "red-yellow.mtl"
                   ]]
    (-> (js/Promise.all (map #(.fetch js/window %) filenames))
        (.then (fn [responses]
                 (js/Promise.all (map #(.text %) responses))))
        (.then (fn [texts]
                 (reset! world/data-map (zipmap filenames texts))
                 (let [button (util/get-by-id :button)]
                   (set! (.-textContent button) "Enter VR")
                   (set! (.-onclick button)
                         #(-> (.requestSession (.-xr js/navigator) "immersive-vr")
                              (.then on-session-started)))))))))

(init) ;; (defonce _ (init))

;; (when @world/reload?
;;   (reset!
;;     world/world
;;     (-> @world/world
;;         (select-keys [:buffers :gl :projection-matrix :programs])
;;         core/create-world)))
