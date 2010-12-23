(defproject marginalia "0.1.0"
  :description "lightweight literate programming for clojure -- inspired by [docco](http://jashkenas.github.com/docco/)"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojars.nakkaya/markdownj "1.0.2b4"]
                 [hiccup "0.3.0"]]
  :dev-dependencies [[lein-clojars "0.5.0-SNAPSHOT"]
                     [jline "0.9.94"]
                     [swank-clojure "1.2.1"]

                     ;; Mindblowing!  But really, it's just because I couldn't get
                     ;; the lein plugin to run without this + lein install / lein deps
                     ;; cycle.
                     [marginalia "0.1.0"]
                     [lein-javac "1.2.1-SNAPSHOT"]]
  :java-source-path "src/")
