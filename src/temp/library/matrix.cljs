(ns temp.library.matrix
  (:require gl-matrix
            [temp.library.util :as util]))

(gl-matrix/glMatrix.setMatrixArrayType js/Array)

(defn get-identity []
  (.create gl-matrix/mat4))

(defn from-rotation [axis angle]
  (let [m (get-identity)]
    (.fromRotation gl-matrix/mat4 m (util/to-radians angle) (clj->js axis))
    m))

(defn multiply [m other]
  (if (vector? other)
    (let [v (.create gl-matrix/vec4)]
      (.transformMat4 gl-matrix/vec4
                      v (clj->js (conj other 1.0)) m)
      (vec (js->clj v)))
    (let [m2 (get-identity)]
      (.multiply gl-matrix/mat4 m2 other m)
      m2)))

(defn invert [m]
  (let [m2 (get-identity)]
    (.invert gl-matrix/mat4 m2 m)
    m2))

(defn get-perspective [fov aspect near far]
  (let [m (get-identity)]
    (.perspective gl-matrix/mat4 m fov aspect 0.1 100)
    m))

(defn look-at [eye center up]
  (let [m (get-identity)]
    (.lookAt gl-matrix/mat4 m
             (clj->js eye)
             (clj->js center)
             (clj->js up))
    m))

(defn scale [matrix scale]
  (let [m (get-identity)]
    (.scale gl-matrix/mat4 m matrix (clj->js scale))
    m))

(defn make-transform
  ([position rotation]
   (make-transform position rotation [1 1 1]))

  ([position rotation scale]
   (let [axis (clj->js (vec (take 3 rotation)))
         angle (util/to-radians (last rotation))
         q (.create gl-matrix/quat)
         matrix (get-identity)]
     (.setAxisAngle gl-matrix/quat q axis angle)
     (.normalize gl-matrix/quat q q)
     (.fromRotationTranslationScale
       gl-matrix/mat4 matrix q (clj->js position) (clj->js scale))
     matrix)))

(defn undo-transform [m]
  (let [q (.create gl-matrix/quat)]
    (.getRotation gl-matrix/mat4 q m)
    (let [axis (.create gl-matrix/vec3)
          angle (util/to-degrees (.getAxisAngle gl-matrix/quat axis q))
          position (.create gl-matrix/vec3)]
      (.getTranslation gl-matrix/mat4 position m)
      {:position (js->clj position)
       :rotation (conj (js->clj axis) angle)})))

(defn combine-transforms [a b]
  (let [ma (make-transform (:position a) (:rotation a))
        mb (make-transform (:position b) (:rotation b))]
    (undo-transform (multiply ma mb))))

(defn remove-transform [a b]
  (let [ma (make-transform (:position a) (:rotation a))
        mb (make-transform (:position b) (:rotation b))
        imb (invert mb)]
    (undo-transform (multiply ma imb))))

(defn apply-transform [{:keys [position rotation]} point]
  (vec (butlast (multiply (make-transform position rotation) point))))
