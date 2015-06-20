(ns somni.marshalling.serializers-test
  (:require [somni.marshalling.serializers :refer :all]
            [clojure.test :refer :all]))

(def sample-accept-header "*/*;q=0.5, application/json, ")
(def picky-accept-header "application/json, javascript/edn;q=0.5")
(def unacceptable-header "text/yaml")

(defn accepts [a] {:headers {"Accept" a}})

(deftest acceptable?-test
  (is (not (acceptable? (accepts unacceptable-header))))
  (is (acceptable? (accepts picky-accept-header)))
  (is (acceptable? (accepts sample-accept-header))))
