(ns temp.core
  (:require [temp.library.world :as world]
            [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.vector :as vector]
            [temp.library.transform :as transform]
            [temp.library.controllers :as controllers]
            [temp.lego :as lego]
            [temp.ship :as ship]
            ))

(defn create-world [world]
  (ship/create-world world))

(defn update-world [world]
  (ship/update-world world))

(defn button-pressed [world event]
  (ship/button-pressed world event))

(defn button-released [world event]
  (ship/button-released world event))
