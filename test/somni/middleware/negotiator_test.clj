(ns somni.middleware.negotiator-test
  (:require [somni.middleware.negotiator :refer :all]
            [clojure.test :refer :all]))

(def test-req {:body "[1 2 3 4]"
               :headers {"accept" "*/*"
                         "content-type" "application/edn"
                         "content-length" "9"}})

(defn test-handler [{:keys [body]}] {:body {:add-result (apply + body)}})

(def wrapped-th (wrap-negotiator test-handler))

(deftest wrap-content-negotiation-test
  (is (= (wrapped-th {:body "[a b c d]",
                      :headers {"content-length" 9}})
         {:status 415,
          :body "Unsupported Content-Type"})

      "Unsupported media type for deserialization")

  (is (= (wrapped-th {:body "[a b c d]",
                      :headers {"accept" "no/way"
                                "content-type" "application/edn"}})
         {:status 406,
          :body "Not Acceptable"})

      "Unacceptable response type for serialization")

  (is (= (wrapped-th test-req)
         {:headers {"Content-Type" "application/json;charset=UTF-8"},
          :body "{\"add_result\":10}"})
      "Correct response returned"))
