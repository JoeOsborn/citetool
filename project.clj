(defproject citetool "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-3211"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [sablono "0.3.4"]
                 [org.omcljs/om "0.8.8"]]

  :plugins [[lein-cljsbuild "1.0.5"]
            [lein-figwheel "0.3.1"]]

  :source-paths ["src/tools"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]
  
  :cljsbuild {
    :builds 
    [{:source-paths ["src/atom"],
         :id "atom-dev",
         :compiler {:output-to "resources/main.js",
                    :optimizations :simple
                    :pretty-print true
                    :cache-analysis true
                    :source-map-timestamp true}}
     {:source-paths ["src/ui"],
         :id "frontend-dev",
         :figwheel { :on-jsload "citetool.ui.core/on-js-reload" }
         :compiler {:output-dir "resources/public/js/ui-out"
                    :output-to "resources/public/js/ui-core.js",
                    :optimizations :none
                    ; :pretty-print true
                    :source-map true
                    :cache-analysis true}}]}
  :figwheel {:css-dirs ["resources/public/css"] ;; watch and update CSS
             :ring-handler figwheel-middleware/app
             ;; To be able to open files in your editor from the heads up display
             ;; you will need to put a script on your path.
             ;; that script will have to take a file path and a line number
             ;; ie. in  ~/bin/myfile-opener
             ;; #! /bin/sh
             ;; emacsclient -n +$2 $1
             ;;
             ;; :open-file-command "myfile-opener"
             })
