(defproject somni "1.0.0-4"
  :description
  "Stop writing Ring handlers; write the functions you want to write."

  :url
  "https://github.com/CareLogistics/somni"

  :license
  {:name "Eclipse Public License 1.0"
   :url "http://opensource.org/licenses/eclipse-1.0.php"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.velisco/herbert "0.6.11"]
                 [prismatic/schema "0.4.0"]
                 [ring/ring-core "1.3.2"]
                 [liberator "0.13"]
                 [camel-snake-kebab "0.3.2"]])
