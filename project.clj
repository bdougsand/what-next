(defproject whats-next "0.1.0-SNAPSHOT"
  :description "A simple time tracker."
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.145"]
                 [org.clojure/core.async "0.2.371"]

                 [org.omcljs/om "0.9.0"]
                 [sablono "0.3.6"]

                 #_[cljsjs/pouchdb "3.4.0-1"]
                 ;; Persist atom to local data store.
                 [alandipert/storage-atom "1.2.4"]]

  :plugins [[lein-cljsbuild "1.0.5"]
            [lein-figwheel "0.4.1"]
            [lein-doo "0.1.6-SNAPSHOT"]]

  :source-paths ["src"]

  :clean-targets [:target-path
                  "resources/public/js/compiled"
                  "target"]

  :cljsbuild {
              :builds [{:id "dev"
                        :source-paths ["src"]

                        :figwheel {:on-jsload "whats-next.core/on-js-reload" }

                        :compiler {:main whats-next.core
                                   :asset-path "js/compiled/out"
                                   :output-to "resources/public/js/compiled/whats_next.js"
                                   :output-dir "resources/public/js/compiled/out"
                                   :optimizations :none
                                   :source-map true
                                   :source-map-timestamp true
                                   :cache-analysis true }}
                       {:id "test"
                        :source-paths ["src" "test"]
                        :compiler {:main whats-next.runner
                                   :output-to "resources/public/js/compiled/testable.js"
                                   :optimizations :none}}
                       {:id "min"
                        :source-paths ["src"]
                        :compiler {:output-to "resources/public/js/compiled/whats_next.js"
                                   :main whats-next.core
                                   :optimizations :advanced
                                   :pretty-print false}}]}

  :figwheel {
             ;; :http-server-root "public" ;; default and assumes "resources"
             :server-port 3450 ;; default
             :css-dirs ["resources/public/css"]})
