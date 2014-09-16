(defproject somni "0.1.3"
  :description "An opinionated yet lightweight services routing library"
  :url "https://github.com/CareLogistics/somni"
  :license {:name "Eclipse Public License 1.0"
            :url "http://opensource.org/licenses/eclipse-1.0.php"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [prismatic/schema "0.2.6"]]

  :profiles
  {:uberjar {:aot :all}

   :test {:dependencies [[bidi "1.10.4"]
                         [compojure "1.1.9"]
                         [ring-mock "0.1.5"]]}})
