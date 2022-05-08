(ns temp.library.window
  (:require [temp.library.util :as util]
            [temp.library.vector :as vector]
            [temp.library.matrix :as matrix]
            [temp.library.world :as world]
            [temp.core :as core]))

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
   :normal (.getAttribLocation gl program "normal")
   :texture-coordinates (.getAttribLocation gl program "textureCoordinates")})

(defn compile-program [gl name]
  (let [vs-source (get @world/data-map (str name "-vert.glsl"))
        vertex-shader (load-shader gl (.-VERTEX_SHADER gl) vs-source)
        fs-source (get @world/data-map (if (= name "textured")
                                         (str name "-frag.glsl")
                                         "frag.glsl"))
        fragment-shader (load-shader gl (.-FRAGMENT_SHADER gl) fs-source)
        program (.createProgram gl)]
    (.attachShader gl program vertex-shader)
    (.attachShader gl program fragment-shader)
    (.linkProgram gl program)
    (.useProgram gl program)
    {:index program
     :locations (create-locations gl program)}))

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
              (if-let [pose (.getPose frame (.-gripSpace input-source) ref-space)]
                (let [transform (.-transform pose)
                      position (.-position transform)
                      rotation (.-orientation transform)
                      gamepad (.-gamepad input-source)
                      hand (keyword (.-handedness input-source))
                      w (update-buttons w hand gamepad)
                      w (assoc-in w [:actuators hand] (first (.-hapticActuators gamepad)))]
                  (core/controller-moved w {:position [(.-x position) (.-y position) (.-z position)]
                                            :rotation (quat->axis-angle [(.-x rotation) (.-y rotation)
                                                                         (.-z rotation) (.-w rotation)])
                                            :hand hand
                                            :buttons (map (fn [button]
                                                            {:pressed (.-pressed button)
                                                             :value (.-value button)
                                                             :touched (.-touched button)})
                                                          (.-buttons gamepad))
                                            :axes (array-seq (.-axes gamepad))}))
                w))
            world (array-seq (.-inputSources session)))))

(defn loop! [time frame]
  (let [session (.-session frame)
        world @world/world
        gl (:gl world)]
    (.requestAnimationFrame session loop!)
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
              (core/draw-world!)))))))

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
      (assoc-in [:programs :colored] (compile-program gl "colored"))
      (assoc-in [:programs :textured] (compile-program gl "textured"))
      (assoc-in [:programs :wireframe] (compile-program gl "wireframe"))
      (assoc-in [:buffers] {:position (.createBuffer gl)
                            :normal (.createBuffer gl)
                            :color (.createBuffer gl)
                            :texture-coordinates (.createBuffer gl)})))

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
                 (.requestAnimationFrame session loop!))))))

(defn init []
  (let [filenames ["frag.glsl"
                   "flat-vert.glsl"
                   "textured-vert.glsl"
                   "textured-frag.glsl"
                   "colored-vert.glsl"
                   "wireframe-vert.glsl"
                   "cube.obj"
                   "cube.mtl"
                   "sphere.obj"
                   "sphere.mtl"
                   "cylinder.obj"
                   "cylinder.mtl"
                   "cone.obj"
                   "cone.mtl"
                   "letters.obj"
                   "letters.mtl"
                   "letters2.obj"
                   "letters2.mtl"
                   "axis.obj"
                   "axis.mtl"
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

(defonce _ (init))

(when @world/reload?
  (println "reloaded")
  (reset!
    world/world
    (-> @world/world
        (select-keys [:actuators
                      :buffers
                      :gl
                      :ref-space
                      :ctx
                      :programs])
        core/create-world)))
