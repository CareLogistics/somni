(ns somni.middleware.negotiator-test
  (:require [somni.middleware.negotiator :refer :all]
            [clojure.test :refer :all]))

(deftest built-in-marshalling-test
  (is (= (clj-> "*/*" [:a 1 :b 2])
         "[:a 1 :b 2]"))

  (is (= (->clj "application/x-www-form-urlencoded" "a=1&b=2")
         {:a 1, :b 2}))

  (is (= (->clj "application/edn" "{a 1 b 2}")
         '{a 1 b 2})))

(deftest private-tests
  (let [realize-body #'somni.middleware.negotiator/realize-body]
    (is (= "body" (:body (realize-body {:body "body"}))))
    (is (= "body" (:body (realize-body {:body (.getBytes "body")}
                                       "UTF-8")))))

  (let [set-content-type #'somni.middleware.negotiator/set-content-type]
    (is (:content-type (set-content-type {} "application/edn")))
    (is (get-in (set-content-type {} "application/edn")
                [:headers "Content-Type"]))))
