(ns temp.library.window
  (:require [temp.library.util :as util]
            [temp.library.world :as world]
            [temp.library.transform :as transform]
            [temp.library.three :as three]
            [temp.library.controllers :as controllers]
            [temp.core :as core]))

(def THREE js/window.THREE)

(defn loop! []
  (if (< @world/update-count 30)
    (let [{:keys [renderer scene camera]} @world/world
          active-fn (or (resolve 'core/keep-active?) identity)]
      (swap! world/world core/update-world)
      (.render renderer scene (:camera camera))
      (swap! world/update-count inc)

      (if (active-fn @world/world)
        (reset! world/update-count 0)))))

(def button (atom nil))

(defn create-listener [handler]
  (fn [event]
    (let [x (.-clientX event)
          y (.-clientY event)
          button-names {1 :left
                        4 :middle
                        2 :right}
          b (or (get button-names (.-buttons event))
                @button)
          e {:x x
             :y y
             :button b}]
      (reset! world/update-count 0)
      (reset! button b)
      (if (not (nil? @world/world))
        (swap! world/world handler e)))))

(defn create-listener2 [handler]
  (fn [event]
    (let [x (.-clientX event)
          y (.-clientY event)
          button-names {1 :left
                        4 :middle
                        2 :right}
          b (or (get button-names (.-buttons event))
                @button)
          e {:x x
             :y y
             :button b}]
      (reset! button b)
      (if (not (nil? @world/world))
        (swap! world/world handler e)))))

(defn create-mouse-listeners! []
  (let [handler #(or % (fn [a b] a))]
    (set! (.-onmousedown js/document)
          (create-listener (handler (resolve 'core/mouse-pressed))))
    (set! (.-onmousemove js/document)
          (create-listener2 (handler (resolve 'core/mouse-moved))))
    (set! (.-onmouseup js/document)
          (create-listener (handler (resolve 'core/mouse-released))))
    (set! (.-onwheel js/document)
          (if-let [f (resolve 'core/mouse-scrolled)]
            (fn [event]
              (let [value (if (pos? (.-deltaY event)) -1 1)]
                (swap! world/world f value)))
            (fn [a b] a)))
    (set! (.-oncontextmenu js/document)
          (fn [event] (.preventDefault event)))))

(defn init []
  (let [scene (new THREE.Scene)
        width (.-innerWidth js/window)
        height (.-innerHeight js/window)
        ratio (/ width height)
        camera (new THREE.PerspectiveCamera 30 ratio 0.1 1000)
        renderer (new THREE.WebGLRenderer #js{:antialias true})
        holder (new THREE.Group)]

    (set! (.-fog scene) (new THREE.Fog 0x001A4B 5 950))
    (.set (.-position camera) 0 0 1)
    (.lookAt camera 0 0 0)

    (set! (.-background scene) (new THREE.Color 0x505050))
    (.setPixelRatio renderer (.-devicePixelRatio js/window))
    (.setSize renderer width height)
    (set! (.-outputEncoding renderer) THREE.sRGBEncoding)
    (set! (.-enabled (.-xr renderer)) true)
    (.setReferenceSpaceType (.-xr renderer) "local")
    (reset! world/world {:scene scene
                         :camera {:position [0 0 0]
                                  :rotation [1 0 0 0]
                                  :object holder
                                  :camera camera}
                         :renderer renderer
                         :clock (new THREE.Clock)
                         :to-load 0})
    (swap! world/world core/create-world)

    ;; (if (and (resolve 'core/button-pressed)
    ;;          (resolve 'core/button-released))
    ;;   (swap! world/world assoc-in [:button-functions]
    ;;          [#(core/button-pressed %1 %2)
    ;;           #(core/button-released %1 %2)]))
    
    (.setAnimationLoop renderer loop!)
    (let [root (.getElementById js/document "root")]
      ;; (.-body js/document)
      (set! (.-innerHTML root) "")
      (.appendChild root (.-domElement renderer)))
    
    (.add holder camera)
    (.add scene holder)

    (create-mouse-listeners!)

    (set! (.-onresize js/window)
          (fn [event]
            (let [width (.-innerWidth js/window)
                  height (.-innerHeight js/window)
                  ratio (/ width height)]
              (println ratio)
              (set! (.-aspect camera) ratio)
              (.updateProjectionMatrix camera)
              (.setSize renderer width height)
              (world/redraw!))))

    (reset! world/update-count 0)

    (controllers/set-index! renderer 0)
    (controllers/set-index! renderer 1)
    1))

(defonce _ (init))

;; (when @world/reload
;;   (.reload (.-location js/window)))


