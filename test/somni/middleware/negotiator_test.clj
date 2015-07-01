(ns somni.middleware.negotiator-test
  (:require [somni.middleware.negotiator :refer :all]
            [clojure.test :refer :all]))

(def test-req {:body "[1 2 3 4]"
               :headers {"accept" "*/*"
                         "content-type" "application/edn"
                         "content-length" "9"}})

(defn test-handler [{:keys [body]}] {:body {:result (apply + body)}})

(def wrapped-th (wrap-negotiator test-handler))

(deftest wrap-content-negotiation-test
  (is (= (:status (wrapped-th {:body "[a b c d]"
                               :headers {"content-length" 9}}))
         415)
      "Unsupported media type for deserialization")

  (is (= (:status (wrapped-th {:body "[a b c d]",
                               :headers {"accept" "no/way"
                                         "content-type" "application/edn"}}))
         406)
      "Unacceptable response type for serialization")

  (is (= (:body (wrapped-th test-req))
         "{\"result\":10}")
      "Correct response returned")

  (is (= (get-in (wrapped-th test-req) [:headers "Content-Type"])
         "application/json;charset=UTF-8")
      "Content-Type set by middleware"))
