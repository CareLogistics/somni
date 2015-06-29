(ns somni.middleware.negotiator-test
  (:require [somni.middleware.negotiator :refer :all]
            [clojure.test :refer :all]))

(deftest built-in-marshalling-test
  (is (= (clj-> *default-mime-type* [:a 1 :b 2])
         "[:a 1 :b 2]"))

  (is (= (->clj "application/x-www-form-urlencoded" "a=1&b=2")
         {:a 1, :b 2}))

  (is (= (->clj "application/edn" "{a 1 b 2}")
         '{a 1 b 2})))

(def test-req {:body "[1 2 3 4]"
               :headers {"Accept" "*/*"
                         "content-type" "application/edn"
                         "content-length" 9}})

(defn test-handler [{:keys [body]}] {:body {:result (apply + body)}})

(def wrapped-th (wrap-content-negotiation test-handler))

(deftest wrap-content-negotiation-test
  (is (= 415 (:status (wrapped-th {:body "[a b c d]"
                                   :headers {"content-length" 9}})))
      "Unsupported media type for deserialization")

  (is (= 406 (:status (wrapped-th {:body "[a b c d]",
                                   :headers {"Accept" "no/way"
                                             "content-type" "application/edn"}})))
      "Unacceptable response type for serialization")

  (is (= "{:result 10}" (:body (wrapped-th test-req)))
      "Correct response returned")

  (is (= "application/edn" (get-in (wrapped-th test-req) [:headers "Content-Type"]))
      "Content-Type set by middleware"))
