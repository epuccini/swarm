(defproject swarm "0.1.0-SNAPSHOT"
  :description "Swarm simulation"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [quil "2.8.0"]]
  :main swarm.core
  :jar-name "swarm.jar"
  :uberjar-name "swarm-STANDALONE.jar"
  :aot [swarm.core])
