(ns temp.library.world)

(def update-count (atom 0))

(defonce world (atom nil))

(defn redraw! []
  (reset! update-count 0))

(defn redraw [world]
  (redraw!)
  world)
