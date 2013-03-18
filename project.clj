(defproject org.blancas/morph "0.2.0"
  :description "A Library of Morphisms: Monoids, Functors, and Monads"
  :license {:name "Eclipse Public License"
	    :url "http://www.eclipse.org/legal/epl-v10.html"}
  :url "https://github.com/blancas/morph"
  :plugins [[codox "0.6.4"]]
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :source-paths ["src/main/clojure" "src/main/resources"]
  :codox {:sources ["src/main/clojure"] :output-dir "codox"}
  :jvm-opts ["-Dfile.encoding=UTF-8"])
