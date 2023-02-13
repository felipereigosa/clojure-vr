(defproject clojure-simple-http "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.11.0"]
                 [org.clojure/clojurescript "1.10.597"]
                 [http-kit "2.3.0"]
		 [compojure "1.6.1"]]

  :profiles {:dev
             {:dependencies [[com.bhauman/figwheel-main "0.2.6"]
                             [com.bhauman/rebel-readline-cljs "0.1.4"]
                             [cljs-ajax "0.8.4"]]
              :resource-paths ["target"]
              :clean-targets ^{:protect false} ["target"]}}
  
  :figwheel {:http-server-root "public"
             :hot-reload-cljs false}

  :plugins [[lein-cljsbuild "1.1.7"]]
  :cljsbuild {:builds [{:source-paths ["src/temp"]
                        :compiler {:output-to "resources/public/cljs-out/dev-main.js"
                                   :optimizations :advanced
                                   :pretty-print true
                                   :install-deps true}}]}
  :java-source-paths ["src/server/java"]
  :main server.core)
