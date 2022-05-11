(ns temp.library.mesh
  (:require [temp.library.vector :as vector]
            [temp.library.matrix :as matrix]
            [temp.library.util :as util :refer [third]]
            [temp.library.world :as world]))

(defn compute-normals [vertices]
  (flatten (map (fn [[a b c]]
                  (let [v1 (vector/subtract b a)
                        v2 (vector/subtract c a)
                        v3 (vector/cross-product v1 v2)
                        nv3 (vector/normalize v3)]
                    (list nv3 nv3 nv3)))
                (partition 3 (partition 3 vertices)))))

(defn rgb [color]
  (let [color (if (keyword? color)
                (name color)
                color)]
    (if (string? color)
      (let [ctx (:ctx @world/world)]
        (set! (.-fillStyle ctx) color)
        (->> (.-fillStyle ctx)
             rest
             (partition 2)
             (map #(/ (js/parseInt (apply str %) 16) 255))))
      (map #(/ % 255) color))))

(defn draw-flat [world mesh]
  (let [{:keys [gl projection-matrix view-matrix camera buffers]} world
        program (get-in world [:programs :flat])
        locations (:locations program)
        buffers (:buffers world)
        {:keys [position rotation color vertex-buffer
                normal-buffer num-vertices scale]} mesh
        [r g b] color
        camera-matrix (:view-matrix camera)
        model-matrix (matrix/from-transform position rotation scale)]

    (.useProgram gl (:index program))
    (.uniformMatrix4fv gl (:projection-matrix locations) false projection-matrix)
    (.uniformMatrix4fv gl (:model-matrix locations) false model-matrix)
    (.uniformMatrix4fv gl (:view-matrix locations) false view-matrix)
    (.uniformMatrix4fv gl (:camera-matrix locations) false camera-matrix)
    (.uniform4f gl (:material-color locations) r g b 1)

    (.bindBuffer gl (.-ARRAY_BUFFER gl) (:position buffers))
    (.vertexAttribPointer gl (:position locations) 3 (.-FLOAT gl)
                          false 0 vertex-buffer)
    (.enableVertexAttribArray gl (:position locations))
    (.bufferData gl (.-ARRAY_BUFFER gl) vertex-buffer (.-STATIC_DRAW gl))

    (.bindBuffer gl (.-ARRAY_BUFFER gl) (:normal buffers))
    (.vertexAttribPointer gl (:normal locations) 3 (.-FLOAT gl)
                          false 0 normal-buffer)
    (.enableVertexAttribArray gl (:normal locations))
    (.bufferData gl (.-ARRAY_BUFFER gl) normal-buffer (.-STATIC_DRAW gl))

    (.drawArrays gl (.-TRIANGLES gl) 0 num-vertices)))

(defn draw-colored [world mesh]
  (let [{:keys [gl projection-matrix view-matrix camera buffers]} world
        program (get-in world [:programs :colored])
        locations (:locations program)
        buffers (:buffers world)
        {:keys [position rotation vertex-buffer normal-buffer
                color-buffer num-vertices scale]} mesh
        camera-matrix (:view-matrix camera)
        model-matrix (matrix/from-transform position rotation scale)]

    (.useProgram gl (:index program))
    (.uniformMatrix4fv gl (:projection-matrix locations) false projection-matrix)
    (.uniformMatrix4fv gl (:model-matrix locations) false model-matrix)
    (.uniformMatrix4fv gl (:view-matrix locations) false view-matrix)
    (.uniformMatrix4fv gl (:camera-matrix locations) false camera-matrix)

    (.bindBuffer gl (.-ARRAY_BUFFER gl) (:position buffers))
    (.vertexAttribPointer gl (:position locations) 3 (.-FLOAT gl)
                          false 0 vertex-buffer)
    (.enableVertexAttribArray gl (:position locations))
    (.bufferData gl (.-ARRAY_BUFFER gl) vertex-buffer (.-STATIC_DRAW gl))

    (.bindBuffer gl (.-ARRAY_BUFFER gl) (:color buffers))
    (.vertexAttribPointer gl (:color locations) 4 (.-FLOAT gl)
                          false 0 color-buffer)
    (.enableVertexAttribArray gl (:color locations))
    (.bufferData gl (.-ARRAY_BUFFER gl) color-buffer (.-STATIC_DRAW gl))

    (.bindBuffer gl (.-ARRAY_BUFFER gl) (:normal buffers))
    (.vertexAttribPointer gl (:normal locations) 3 (.-FLOAT gl)
                          false 0 normal-buffer)
    (.enableVertexAttribArray gl (:normal locations))
    (.bufferData gl (.-ARRAY_BUFFER gl) normal-buffer (.-STATIC_DRAW gl))

    (.drawArrays gl (.-TRIANGLES gl) 0 num-vertices)))

(defn draw-textured [world mesh]
  (let [{:keys [gl projection-matrix view-matrix camera buffers]} world
        program (get-in world [:programs :textured])
        locations (:locations program)
        buffers (:buffers world)
        {:keys [position rotation vertex-buffer normal-buffer
                texture texture-coordinates-buffer
                num-vertices scale]} mesh
        camera-matrix (:view-matrix camera)
        model-matrix (matrix/from-transform position rotation scale)]
    (.useProgram gl (:index program))
    (.uniformMatrix4fv gl (:projection-matrix locations) false projection-matrix)
    (.uniformMatrix4fv gl (:model-matrix locations) false model-matrix)
    (.uniformMatrix4fv gl (:view-matrix locations) false view-matrix)
    (.uniformMatrix4fv gl (:camera-matrix locations) false camera-matrix)

    (.bindBuffer gl (.-ARRAY_BUFFER gl) (:position buffers))
    (.vertexAttribPointer gl (:position locations) 3 (.-FLOAT gl)
                          false 0 vertex-buffer)
    (.enableVertexAttribArray gl (:position locations))
    (.bufferData gl (.-ARRAY_BUFFER gl) vertex-buffer (.-STATIC_DRAW gl))

    (.bindBuffer gl (.-ARRAY_BUFFER gl) (:normal buffers))
    (.vertexAttribPointer gl (:normal locations) 3 (.-FLOAT gl)
                          false 0 normal-buffer)
    (.enableVertexAttribArray gl (:normal locations))
    (.bufferData gl (.-ARRAY_BUFFER gl) normal-buffer (.-STATIC_DRAW gl))

    (.bindTexture gl (.-TEXTURE_2D gl) texture)
    (.bindBuffer gl (.-ARRAY_BUFFER gl) (:texture-coordinates buffers))
    (.vertexAttribPointer gl (:texture-coordinates locations) 2 (.-FLOAT gl)
                          false 0 texture-coordinates-buffer)
    (.enableVertexAttribArray gl (:texture-coordinates locations))
    (.bufferData gl (.-ARRAY_BUFFER gl) texture-coordinates-buffer
                 (.-STATIC_DRAW gl))

    (.drawArrays gl (.-TRIANGLES gl) 0 num-vertices)))

(defn load-texture []
  (let [gl (:gl @world/world) ;;###################
        texture (.createTexture gl)]
    (.bindTexture gl (.-TEXTURE_2D gl) texture)
    (.texImage2D gl (.-TEXTURE_2D gl) 0 (.-RGBA gl)
                 (.-RGBA gl) (.-UNSIGNED_BYTE gl) world/image)
    (.generateMipmap gl (.-TEXTURE_2D gl))
    texture))

(defn create [vertices position rotation scale
              skin normals texture-coordinates]
  (let [vertices (flatten vertices)
        scale (if (vector? scale)
                scale
                (vec (repeat 3 scale)))
        normals (or normals (compute-normals vertices))
        skin (cond
               (string? skin)
               {:texture (load-texture)
                :texture-coordinates-buffer
                (js/Float32Array. (vec (flatten texture-coordinates)))
                :draw-fn draw-textured}

               (and (vector? skin) (vector? (first skin)))
               {:color-buffer (js/Float32Array. (vec (flatten skin)))
                :draw-fn draw-colored}

               :else
               {:color (rgb skin)
                :draw-fn draw-flat})]
    (merge skin
           {:position position
            :rotation rotation
            :vertex-buffer (js/Float32Array. vertices)
            :normal-buffer (js/Float32Array. (vec normals))
            :num-vertices (/ (count vertices) 3)
            :scale scale})))

(defn draw-wireframe [world mesh]
  (let [{:keys [gl projection-matrix view-matrix camera buffers]} world
        program (get-in world [:programs :wireframe])
        locations (:locations program)
        buffers (:buffers world)
        {:keys [position rotation color vertex-buffer
                num-vertices scale]} mesh
        [r g b] color
        camera-matrix (:view-matrix camera)
        model-matrix (matrix/from-transform position rotation scale)]
    (.useProgram gl (:index program))
    (.uniformMatrix4fv gl (:projection-matrix locations) false projection-matrix)
    (.uniformMatrix4fv gl (:model-matrix locations) false model-matrix)
    (.uniformMatrix4fv gl (:view-matrix locations) false view-matrix)
    (.uniformMatrix4fv gl (:camera-matrix locations) false camera-matrix)
    (.uniform4f gl (:material-color locations) r g b 1)

    (.bindBuffer gl (.-ARRAY_BUFFER gl) (:position buffers))
    (.vertexAttribPointer gl (:position locations) 3 (.-FLOAT gl)
                          false 0 vertex-buffer)
    (.enableVertexAttribArray gl (:position locations))
    (.bufferData gl (.-ARRAY_BUFFER gl) vertex-buffer (.-STATIC_DRAW gl))

    (.drawArrays gl (.-LINES gl) 0 num-vertices)))

(defn create-wireframe [vertices position rotation scale color]
  (let [vertices (flatten vertices)
        scale (if (vector? scale)
                scale
                (vec (repeat 3 scale)))]
    {:vertex-buffer (js/Float32Array. vertices)
     :num-vertices (/ (count vertices) 3)
     :position position
     :rotation rotation
     :color (rgb color)
     :scale scale
     :draw-fn draw-wireframe}))

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
    (into [] (concat z-parallel x-parallel))))

(defn parse-line [line]
  (map js/parseFloat
       (rest (.split line " "))))

(defn parse-line-with-slashes [line]
  (map #(map js/parseFloat (.split % "/"))
       (rest (.split line " "))))

(defn use-indices [vec indices]
  (map (fn [v]
         (nth vec (dec v)))
       indices))

(defn parse-material [lines]
  (let [name (subs (util/find-line lines "newmtl") 7)
        texture-line (util/find-line lines "map_Kd")]
    {name {:diffuse (parse-line (util/find-line lines "Kd"))
           :texture (if texture-line
                      (subs texture-line 7))}}))

(defn parse-materials [filename]
  (let [lines (world/read-lines filename)
        lines (filter (fn [line]
                        (or (.startsWith line "newmtl")
                            (.startsWith line "Kd")
                            (.startsWith line "map_Kd")))
                      lines)
        materials (util/create-groups #(.startsWith % "newmtl") lines)]
    (apply merge (cons {"white" {:diffuse [1 1 1]
                                 :texture nil}}
                       (map parse-material materials)))))

(defn create-colors [lines materials]
  (let [material-lines (filter (fn [[index line]]
                                 (.startsWith line "usemtl"))
                               (map vector (range) lines))]
    (vec (mapcat (fn [[i material] [j _]]
                   (let [name (subs material 7)
                         color (get-in materials [name :diffuse])]
                     (repeat (* (- j i 1) 3) (conj (vec color) 1))))
                 material-lines
                 (conj (vec (rest material-lines))
                       [(count lines) nil])))))

(defn create-model
  ([filename position rotation scale color]
   (let [materials-filename (-> filename
                                (subs 0 (.lastIndexOf filename "."))
                                (str ".mtl"))
         materials (parse-materials materials-filename)
         lines (world/read-lines filename)]
     (create-model lines materials position rotation scale color)))
  ([lines materials position rotation scale color]
   (let [v (map parse-line (filter #(.startsWith % "v ") lines))
         n (map parse-line (filter #(.startsWith % "vn ") lines))
         t (map parse-line (filter #(.startsWith % "vt ") lines))
         faces (map parse-line-with-slashes
                    (filter #(.startsWith % "f") lines))
         faces-nth (fn [n]
                     (map (fn [line]
                            (map #(nth % n) line))
                          faces))
         vertices (flatten (map #(use-indices v %) (faces-nth 0)))
         normals (flatten (map #(use-indices n %) (faces-nth 2)))

         texture-name (some :texture (vals materials))
         texture-coordinates (if texture-name
                               (flatten (map (fn [[u v]]
                                               [u (- 1.0 v)])
                                             (mapcat #(use-indices t %) (faces-nth 1))))
                               [])
         skin (or color
                  texture-name
                  (create-colors lines materials))]
     (create vertices position rotation scale skin
             normals texture-coordinates))))

(defn draw [world mesh]
  (when mesh
    ((:draw-fn mesh) world mesh)))
