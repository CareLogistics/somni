(ns somni.middleware.access-control-test
  (:require [somni.middleware.access-control :refer [wrap-authorization]]
            [clojure.test :refer :all]))

(def ^:private test-user {:user 'test, :roles [:admin]})

(defn- h [{:as request :keys [identity]}]
  (if (= identity test-user)
    {:status 200}
    (throw (ex-info "Missing user identity" {}))))

(def test-req {:request-method :get
               :identity test-user})

(def test-acl [:admin])

(deftest wrap-authorization-test
  (is (= 200 (:status ((wrap-authorization h test-acl) test-req)))
      "Authorization allows access when user has role")

  (is (= 403 (:status ((wrap-authorization h test-acl) {:request-method :get})))
      "Access denied when identity is unknown")

  (is (= 403 (:status ((wrap-authorization h nil) test-req)))
      "Access denied when no roles assigned to resource"))
