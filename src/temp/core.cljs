(ns temp.core
  (:require [temp.library.world :as world]
            [temp.library.three :as three]
            [temp.library.util :as util :refer [dissoc-in]]
            [temp.library.vector :as vector]
            [temp.library.transform :as transform]
            [temp.library.camera :as camera]))

(do
1

(defn create-world [world]
  (-> world
      (three/set-clear-color [0 128 204])
      (three/create-lights [8 1 0] true)
      (update-in [:camera] #(merge % {:distance 10
                                      :x-angle 25
                                      :y-angle -35
                                      :pivot [0 0 0]}))
      camera/compute
      (three/create-cube [:background-meshes :ground]
                         [0 -0.101 0] [1 0 0 0] [12 0.2 12] [10 10 10])
      (three/create-wireframe [:background-meshes :grid]
                              (three/get-grid-vertices 12 1) [2 2 2] 2)
      ;;--
      (three/create-cube [:meshes :cube] [0 0.5 0] [1 0 0 0] 1 :yellow)
      (three/create-cube [:meshes :cube] [1 1 0] [1 0 0 0] [1 2 1] :green)
      ))

(defn reload! []
  (dotimes [i 10] ;;################################ why?
    (let [scene (:scene @world/world)]
      (doseq [child (.-children scene)]
        (if (not (.-isGroup child))
          (.remove scene child)))))
  
  (swap! world/world create-world)
  (reset! world/update-count 0)
  (.clear js/console)
  nil)

;; (reload!)
)

(defn update-world [world]
  world)

(defn mouse-pressed [world {:keys [x y]}]
  (assoc-in world [:last-point] [x y]))

(defn mouse-moved [world event]
  (if (not (nil? (:last-point world)))
    (camera/rotate world event)
    world))

(defn mouse-released [world event]
  (dissoc-in world [:last-point]))

(defn mouse-scrolled [world amount]
  (camera/zoom world amount))

(defn keep-active? [world]
  (not (nil? (:last-point world))))

