(ns temp.library.transform
  (:require [temp.library.matrix :as matrix]))

(defn invert [{:keys [position rotation]}]
  (-> (matrix/make-transform position rotation)
      (matrix/invert)
      (matrix/undo-transform)))
