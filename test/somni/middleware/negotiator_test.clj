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

(deftest private-tests
  (let [realize-body #'somni.middleware.negotiator/realize-body]
    (is (= "body" (realize-body "body")))
    (is (= "body" (realize-body (.getBytes "body") "UTF-8"))))

  (let [set-content-type #'somni.middleware.negotiator/set-content-type]
    (is (get-in (set-content-type {} "application/edn")
                [:headers "Content-Type"]))))

(def test-req {:content-type "application/edn"
               :body "[1 2 3 4]"
               :headers {"Accept" "*/*"}})

(defn test-handler [{:keys [body]}] {:body {:result (apply + body)}})

(def wrapped-th (wrap-content-negotiation test-handler))

(deftest wrap-content-negotiation-test
  (is (= 415 (:status (wrapped-th {:body "[a b c d]", :content-type "no/way"})))
      "Unsupported media type for deserialization")

  (is (= 406 (:status (wrapped-th {:body "[a b c d]", :content-type "application/edn"
                                   :headers {"Accept" "no/way"}})))
      "Unacceptable response type for serialization")

  (is (= "{:result 10}" (:body (wrapped-th test-req)))
      "Correct response returned")

  (is (= "application/edn" (get-in (wrapped-th test-req) [:headers "Content-Type"]))
      "Content-Type set by middleware"))
