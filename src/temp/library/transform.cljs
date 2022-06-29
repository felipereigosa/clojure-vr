(ns temp.library.transform
  (:refer-clojure :exclude [apply remove])
  (:require [temp.library.util :as util]))

(def THREE js/window.THREE)

(defn aa->quat [[x y z angle]]
  (let [q (new THREE.Quaternion)]
    (.setFromAxisAngle q (.normalize (new THREE.Vector3 x y z))
                       (util/to-radians angle))))

(defn quat->aa [q]
  (let [v (new THREE.Vector4)]
    (.setAxisAngleFromQuaternion v q)
    [(.-x v) (.-y v) (.-z v) (util/to-degrees (.-w v))]))

(defn transform->matrix [{:keys [position rotation] :or
                          {position [0 0 0]
                           rotation [1 0 0 0]}}]
  (let [q (aa->quat rotation)
        [x y z] position
        p (new THREE.Vector3 x y z)
        s (new THREE.Vector3 1 1 1)
        m (new THREE.Matrix4)]
    (.compose m p q s)))

(defn matrix->transform [m]
  (let [p (new THREE.Vector3)
        q (new THREE.Quaternion)]
    (.setFromMatrixPosition p m)
    (.setFromRotationMatrix q m)
    {:position [(.-x p) (.-y p) (.-z p)]
     :rotation (quat->aa q)}))

(defn combine [a b]
  (let [ma (transform->matrix a)
        mb (transform->matrix b)
        m (new THREE.Matrix4)]
    (matrix->transform (.multiplyMatrices m ma mb))))

(defn remove [a b]
  (let [ma (transform->matrix a)
        mb (transform->matrix b)
        m (new THREE.Matrix4)]
    (.invert ma)
    (matrix->transform (.multiplyMatrices m ma mb))))

(defn apply [t [x y z]]
  (let [p (new THREE.Vector3 x y z)
        m (transform->matrix t)]
    (.applyMatrix4 p m)
    [(.-x p) (.-y p) (.-z p)]))

(defn invert [t]
  (let [m (transform->matrix t)]
    (.invert m)
    (matrix->transform m)))
