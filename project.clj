(defproject geom-demos "fractals"
  :description  "none yet"
  :url          "none yet"
  :license      {:name "Apache Software License"
                 :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha5"]
                 [org.clojure/clojurescript "1.9.89"]
                 [thi.ng/geom "0.0.908"]
                 [thi.ng/domus "0.3.0-SNAPSHOT"]
                 [shodan "0.4.2"]]

  :plugins [[lein-cljsbuild "1.1.3"]]

  :clean-targets ^{:protect false} ["resources/public/js"]

  :cljsbuild    {:builds [{:id "dev"
                           :source-paths ["src"]
                           :compiler {:output-to "resources/public/js/app.js"
                                      :optimizations :whitespace
                                      :pretty-print true
                                      }}
                          {:id "prod"
                           :source-paths ["src"]
                           :compiler {:output-to "resources/public/js/app.js"
                                      :optimizations :advanced
                                      ;;:pseudo-names true
                                      ;;:pretty-print true
                                      :pretty-print false
                                      }}]
                 :test-commands {"unit-tests" ["phantomjs" :runner "resources/public/js/app.js"]}})
