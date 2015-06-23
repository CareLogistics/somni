(ns somni.middleware.access-control-test
  (:require [somni.middleware.access-control :refer :all]
            [clojure.test :refer :all]))

(def ^:private test-user {:user 'test, :roles '#{tester}})

(defmethod request->identity :test [& _] test-user)

(defn- h [{:as request :keys [identity]}]
  (if (= identity test-user)
    {:status 200}
    (throw (ex-info "Missing user identity" {}))))

(deftest wrap-authentication-test
  (is (= test-user (:identity ((wrap-authentication h :test) {})))
      "Authenticated requests retain identity through request & response")

  (is (= 401 (:status ((wrap-authentication h :ssl) {})))
      "Unknown auth-types produce not-authenticated")

  (is (= (*anonymous* {})
         (:identity ((wrap-authentication identity :none) {})))
      ":none authentication produces *anonymous* identity"))

(def test-req {:request-method :get
               :identity test-user})

(def test-acl {:get [:tester]})

(deftest wrap-authorization-test
  (is (= 200 (:status ((wrap-authorization h test-acl) test-req)))
      "Authorization allows access when user has role")

  (is (= 403 (:status ((wrap-authorization h test-acl) {:request-method :get})))
      "Access denied when identity is unknown")

  (is (= 403 (:status ((wrap-authorization h nil) test-req)))
      "Access denied when no roles assigned to resource"))
