(ns somni.http.errors-test
  (:require [somni.http.errors :refer :all]
            [clojure.test :refer :all]))

(deftest server-error-test
  (is (= 500 (:status (server-error {})))
      "General server error")

  (is (= 555 (:status (server-error {:status 555})))
      "Handler specified server error codes are maintaied")

  (is (= "{}" (:body (server-error {} :dev-mode)))
      "Request is serializaed to body in dev-mode"))
