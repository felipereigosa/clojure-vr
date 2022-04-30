(ns temp.library.world)

(defonce world (atom {}))
(def reload? (atom false))

(defonce data-map (atom nil))

(defn read-lines [filename]
  (.split (get @data-map filename) "\n"))

(defn set-thing! [path value]
  (if (= path [])
    (reset! world value)
    (swap! world assoc-in path value))
  nil)

(defn get-thing! [path]
  (if (= path [])
    @world
    (get-in @world path)))

(defmacro update-thing! [path fn]
  (let [val-name (gensym 'val)]
    `(set-thing! ~path (let [~val-name (get-thing! ~path)]
                         ~(cons fn (list val-name))))))

;; (println ">" (set-thing! [:meshes :controller-right :color] '(1 0 0)))
