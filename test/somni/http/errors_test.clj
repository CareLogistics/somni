(ns somni.http.errors-test
  (:require [somni.http.errors :refer :all]
            [clojure.test :refer :all]))

(deftest server-error-test
  (is (= (:status (server-error {}))
         500)
      "General server error")

  (is (= (:status (server-error {:status 555}))
         555)
      "Handler specified server error codes are maintaied")

  (is (= (server-error {:request-method :get} :dev-mode)
         {:status 500, :request-method :get})
      "Request is serializaed to body in dev-mode"))
