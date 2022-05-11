(ns temp.library.camera
  (:require [temp.library.vector :as vector]
            [temp.library.matrix :as matrix]
            [temp.library.transform :as transform]))

(defn compute-view-matrix [{:keys [position rotation] :as camera}]
  (let [direction (transform/apply {:position [0 0 0]
                                    :rotation rotation}
                                   [0 0 -1])
        view-matrix (matrix/look-at position
                                    (vector/add position direction)
                                    [0 1 0])]
    (-> camera
        (assoc-in [:view-matrix] view-matrix)
        (assoc-in [:direction] direction))))

(defn create [position rotation]
  (compute-view-matrix {:position position
                        :rotation rotation}))

(defn move [camera new-position]
  (-> camera
      (assoc-in [:position] new-position)
      compute-view-matrix))

(defn rotate [camera angle]
  (let [rotation {:rotation [0 1 0 angle]
                  :position [0 0 0]}
        camera-rotation {:rotation (:rotation camera)
                         :position [0 0 0]}
        final-rotation (transform/combine rotation camera-rotation)]
    (-> camera
        (assoc-in [:rotation] (:rotation final-rotation))
        compute-view-matrix)))
