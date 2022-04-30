(ns temp.library.util
  (:require [cljs.pprint]))

(defn dekeyword [k]
  (subs (str k) 1))

(defn get-by-class [class-name]
  (.getElementsByClassName js/document (dekeyword class-name)))

(defn get-by-id [id]
  (.getElementById js/document (dekeyword id)))

(defn parse-float [text]
  (js/parseFloat text))

(defn find-if [pred coll]
  (first (filter pred coll)))

(defn sleep [duration]
  (dorun
   (let [now (.getTime (new js/Date))]
     (while (< (.getTime (new js/Date)) (+ now duration))))))

(def pi Math/PI)

(defn to-radians [angle]
  (* angle (/ pi 180.0)))

(defn to-degrees [angle]
  (* angle (/ 180.0 pi)))

(defn vec->fa [vec]
  (js/Float32Array. vec))

(defn fa->vec [fa]
  (vec (.call (.-slice (.-prototype js/Array)) fa)))

(defn dissoc-in [map keys]
  (if (= (count keys) 1)
    (dissoc map (nth keys 0))
    (update-in map (butlast keys) dissoc (last keys))))

(defn within [value min max]
  (cond
    (< value min) min
    (> value max) max
    :else value))

(def not-nil? (comp not nil?))

(defn create-groups-helper [acc header? lines]
  (if (empty? lines)
    acc
    (let [line (first lines)]
      (if (header? line)
        (recur (conj acc [line])
               header?
               (rest lines))
        (recur (update-in acc [(dec (count acc))] #(conj % line))
               header?
               (rest lines))))))

(defn create-groups [header? lines]
  (create-groups-helper
    [] header? (drop-while (comp not header?) lines)))

(defn find-line [lines start]
  (find-if #(.startsWith % start) lines))

(def third #(nth % 2))
(def fourth #(nth % 3))

(defn pprint [& args]
  (if (= (count args) 1)
    (cljs.pprint/pprint (first args))
    (cljs.pprint/pprint args)))

(def clear-console #(.clear js/console))

(defn map-map [func m]
  (apply merge (map func m)))

(defn float= [a b]
  (and
   (number? a)
   (number? b)
   (< (Math/abs (- a b)) 0.0001)))

(defn join-keywords [& keywords]
  (keyword (apply str (interpose "-" (map (fn [k]
                                            (subs (str k) 1)) keywords)))))
