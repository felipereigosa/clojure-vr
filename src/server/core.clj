
(ns server.core
  (:require [org.httpkit.server :refer [run-server]]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.util.response :as response]))

(defroutes main-routes
  (GET "/" [] (response/redirect "/index.html"))
  (route/resources "/")
  (route/not-found "Page not found"))

(defn -main [& args]
  (run-server main-routes {:port 8081})
  (println "Server started"))
