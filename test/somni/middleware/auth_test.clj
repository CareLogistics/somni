(ns somni.middleware.auth-test
  (:require [somni.middleware.auth :refer :all]
            [somni.middleware.auth.backends :refer :all]
            [buddy.auth.protocols :as buddy-proto]
            [clojure.test :refer :all]))

(def ^:private test-user {:user 'test, :roles [:admin]})

;;mock buddy authentication backend
(defn mock-backend []
  (reify
    buddy-proto/IAuthentication
    (parse [_ request] test-user)
    (authenticate [_ request data]
      (assoc request :identity data))))

(defn- h [{:as request :keys [identity]}]
  (if (= identity test-user)
    {:status 200}
    (throw (ex-info "Missing user identity" {}))))

(deftest wrap-authentication-test
  (is (= test-user (:identity ((wrap-authentication h (mock-backend)) {})))
      "Authenticated requests retain identity through request & response"))

(deftest get-authn-backend-test
  (is (thrown? clojure.lang.ExceptionInfo
               (get-authn-backend {:type :ssl})
               "Unknown auth-backend-types produce an exception at startup")))

