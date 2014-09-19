(defproject somni "0.2.1"
  :description "An opinionated yet lightweight services routing library"
  :url "https://github.com/CareLogistics/somni"
  :license {:name "Eclipse Public License 1.0"
            :url "http://opensource.org/licenses/eclipse-1.0.php"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [prismatic/schema "0.2.6"]]

  :profiles
  {:test {:dependencies [[compojure "1.1.9"]
                         [ring-mock "0.1.5"]
                         [criterium "0.4.3"]]}})
