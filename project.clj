(defproject somni "0.3.5"
  :description "An opinionated yet lightweight services routing library"
  :url "https://github.com/CareLogistics/somni"
  :license {:name "Eclipse Public License 1.0"
            :url "http://opensource.org/licenses/eclipse-1.0.php"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [prismatic/schema "0.3.0"]]

  :profiles
  {:test {:dependencies [[ring-mock "0.1.5"]
                         [criterium "0.4.3"]]}})
