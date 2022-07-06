(ns temp.core
  (:require [temp.library.world :as world]
            [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.vector :as vector]
            [temp.library.transform :as transform]
            [temp.library.controllers :as controllers]
            [temp.lego :as lego]
            ))

(defn create-world [world]
  (lego/create-world world))

(defn update-world [world]
  (lego/update-world world))

(defn button-pressed [world event]
  (lego/button-pressed world event))

(defn button-released [world event]
  (lego/button-released world event))
