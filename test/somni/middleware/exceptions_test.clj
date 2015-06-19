(ns somni.middleware.exceptions-test
  (:require [somni.middleware.exceptions :refer :all]
            [clojure.test :refer :all]
            [somni.http.errors :refer [server-error]]))

(def ^:private exceptional-handler (constantly (ex-info "Boom" {})))
(def ^:private throwing-handler (fn [_] (throw (exceptional-handler))))

(deftest ex-details-test
  (let [e (ex-details (ex-info "Double Boom" {} (exceptional-handler)))]
    (is (= (:exception e) clojure.lang.ExceptionInfo))
    (is (= (:message e) "Double Boom"))
    (is (:stackTrace e))
    (is (:cause e))
    (is (= (get-in e [:cause :message]) "Boom"))))

(def ^:private weh (wrap-uncaught-exceptions exceptional-handler))
(def ^:private wth (wrap-uncaught-exceptions throwing-handler
                                             #(server-error % :dev-mode)))

(deftest wrap-uncaught-exceptions-test
  (is (= 500 (:status (weh {}))))
  (is (= "Internal server error" (:body (weh {}))))
  (let [r (wth {})]
    (is (= (:status r) 500))
    (is (not= (:body r) "Internal server error"))))
