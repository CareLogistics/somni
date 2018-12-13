(ns somni.middleware.negotiator-test
  (:require [somni.middleware.negotiator :refer :all]
            [clojure.test :refer :all]))

(defn test-handler [{:keys [body]}] {:body {:add-result (apply + body)}})

(def test-req {:body "[1 2 3 4]"
               :headers {"accept" "*/*"
                         "content-type" "application/edn"
                         "content-length" "9"}})

(def wrapped-1 (wrap-negotiator test-handler))

(deftest wrap-content-negotiation-test
  (is (= (wrapped-1 {:body "...",
                      :headers {"content-length" 9}})
         {:status 415,
          :body "\"Unsupported Content-Type\""
          :headers {"Content-Type" "application/json;charset=UTF-8"}})

      "Unsupported media type for deserialization")

  (is (= (wrapped-1 {:body "...",
                      :headers {"accept" "no/way"
                                "content-type" "application/edn"}})
         {:status 406,
          :body "Not Acceptable"})

      "Unacceptable response type for serialization")

  (is (= (wrapped-1 test-req)
         {:headers {"Content-Type" "application/json;charset=UTF-8"},
          :body "{\"addResult\":10}"})
      "Correct response returned"))

(def wrapped-2
  (wrap-negotiator
   (fn [{:keys [body]}]
     {:body body
      :headers {:deserialized body}})))

(deftest test-json-deserialize
  (is (= (wrapped-2 {:body "{\"alphaBeta\":1,\"beta_max\":2}"
                     :headers {"accept" "*/*"
                               "content-type" "application/json"
                               "content-length" 28}})
         {:body "{\"alphaBeta\":1,\"betaMax\":2}",
          :headers {"Content-Type" "application/json;charset=UTF-8",
                    :deserialized {:alpha-beta 1,
                                   :beta-max   2}}})))
