(ns temp.library.canvas
  (:require [temp.library.world :refer [world]]
            [temp.library.util :as util]))

(defn open-image [filename]
  (let [image (new js/Image)]
    (set! (.-src image) filename)
    image))

(defn set-color! [color]
  (let [color (if (keyword? color)
                (name color)
                color)]
    (set! (.-strokeStyle (:ctx @world)) color)
    (set! (.-fillStyle (:ctx @world)) color)))

(defn draw-rect! [color x y w h]
  (let [ctx (:ctx @world)]
    (set-color! color)
    (.beginPath ctx)
    (.rect ctx (- x (/ w 2)) (- y (/ h 2)) w h)
    (.stroke ctx)))

(defn fill-rect! [color x y w h]
  (set-color! color)
  (.fillRect (:ctx @world) (- x (/ w 2)) (- y (/ h 2)) w h))

(defn draw-circle! [color x y r]
  (set-color! color)
  (let [ctx (:ctx @world)]
    (.beginPath ctx)
    (.arc ctx x y r 0 (* 2 util/pi))
    (.stroke ctx)))

(defn fill-circle! [color x y r]
  (set-color! color)
  (let [ctx (:ctx @world)]
    (.beginPath ctx)
    (.arc ctx x y r 0 (* 2 util/pi))
    (.fill ctx)))

(defn draw-image! [image x y]
  (when image
    (.drawImage (:ctx @world) image x y)))
