(merge-env!
 :resource-paths #{"src"}
 :dependencies '[[org.clojure/clojure "1.6.0" :scope "provided"]
                 [prismatic/schema "0.3.0"]
                 [compojure "1.1.9" :scope "test"]
                 [ring-mock "0.1.5" :scope "test"]
                 [criterium "0.4.3" :scope "test"]])

(deftask build
  "Builds"
  []
  (comp
   (pom :project 'somni
        :description "An opinionated yet lightweight services routing library"
        :version "0.3.4"
        :url "https://github.com/CareLogistics/somni"
        :license {"name" "Eclipse Public License 1.0"
                  "url" "http://opensource.org/licenses/eclipse-1.0.php"})
   (jar)))
