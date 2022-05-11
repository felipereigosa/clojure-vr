(ns temp.library.transform
  (:require gl-matrix
            [temp.library.matrix :as matrix]
            [temp.library.util :as util]))

(defn from-matrix [m]
  (let [q (.create gl-matrix/quat)]
    (.getRotation gl-matrix/mat4 q m)
    (let [axis (.create gl-matrix/vec3)
          angle (util/to-degrees (.getAxisAngle gl-matrix/quat axis q))
          position (.create gl-matrix/vec3)]
      (.getTranslation gl-matrix/mat4 position m)
      {:position (js->clj position)
       :rotation (conj (js->clj axis) angle)})))

(defn combine [a b]
  (let [ma (matrix/from-transform (:position a) (:rotation a))
        mb (matrix/from-transform (:position b) (:rotation b))]
    (from-matrix (matrix/multiply ma mb))))

(defn subtract [a b]
  (let [ma (matrix/from-transform (:position a) (:rotation a))
        mb (matrix/from-transform (:position b) (:rotation b))
        imb (matrix/invert mb)]
    (from-matrix (matrix/multiply ma imb))))

(defn apply [{:keys [position rotation]} point]
  (vec (butlast (matrix/multiply (matrix/from-transform position rotation) point))))

(defn invert [{:keys [position rotation]}]
  (-> (matrix/from-transform position rotation)
      (matrix/invert)
      from-matrix))
