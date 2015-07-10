(ns somni-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [somni.middleware.access-control :refer [request->identity]]
            [somni :refer :all]
            [somni.middleware.auth.backends :refer :all]
            [buddy.auth.protocols :as buddy-proto]))

(defn hello [name body db]
  (cond
   body body
   name (db "Hello " name)
   :else "Hello from Somni"))

(def +new-user+ {:schema {:username s/Str}})

(defn ^+new-user+ new-user
  "Creates a new user with uid"
  ([uid body identity db]
   (db :new uid :username (:username body) :by identity))

  ([body identity db]
   (new-user (java.util.UUID/randomUUID) body identity db)))

(defn delete-user
  [uid identity db]
  (db :delete uid :by identity))

(defn get-user
  [uid identity db]
  (db :get uid :by identity))

#_(defmethod request->identity :trusting [_ request] (:identity request))

;;define a test buddy backend
(defn trusting-backend []
  (reify
    buddy-proto/IAuthentication
    (parse [_ request]
      (:identity request))
    (authenticate [_ request data]
      (assoc request :identity data))))

(defmethod get-authn-backend :trusting [m] (trusting-backend))


(def sample-resources
  [
   {:uri   "hello/:name"
    :doc   "Hello Somni example"
    :any   #'hello}

   {:uri   "hello"
    :doc   "Hello Somni example"
    :any   #'hello}

   {:uri    "user/:uid"
    :doc    "User management"
    :put    #'new-user
    :delete #'delete-user
    :get    #'get-user
    :authentication :trusting
    :authorization {:get    #{:admin :user}
                    :delete #{:admin}
                    :put    #{:admin}
                    :post   #{:admin}}}

   {:uri   "user"
    :doc   "Add a user, with server generated uid"
    :post  #'new-user
    :authentication :trusting
    :authorization {:post #{:admin}}}])

(deftest somni-tests
  (let [somni-handler (build sample-resources {:db str})]

    (is (= "Hello Philip J. Fry"
           (:body (somni-handler {:uri "hello"
                                  :request-method :get
                                  :params {:name "Philip J. Fry"}}))
           (:body (somni-handler {:uri "hello/Philip J. Fry"
                                  :request-method :get})))
        "Basic handler test")

    (is (= 401
           (:status (somni-handler {:uri "user", :request-method :post})))
        "Authentication failure test")

    (is (= 403
           (:status (somni-handler {:uri "user",
                                    :request-method :post
                                    :identity {:user "pete",}})))
        "Authorization failure test")

    (is (= 400
           (:status (somni-handler {:uri "user",
                                    :request-method :post
                                    :identity {:user "pete", :roles [:admin]}})))
        "Validation failure test")

    (is (= 415
           (:status (somni-handler {:uri "user",
                                    :request-method :post
                                    :identity {:user "pete", :roles [:admin]}
                                    :headers {"content-type" "application/xml"
                                              "content-length" 25}
                                    :body "<username>john</username>"})))
        "Unsupported media type failure test")

    (is (= 406
           (:status (somni-handler {:uri "user",
                                    :request-method :post
                                    :identity {:user "pete", :roles [:admin]}
                                    :body "{:username \"john\"}"
                                    :headers {"accept" "application/hal+yaml"
                                              "content-type" "application/edn"}})))
        "Not acceptable failure test")

    (is (= 200
           (:status (somni-handler {:uri "user",
                                    :request-method :post
                                    :identity {:user "pete", :roles [:admin]}
                                    :body "{:username \"john\"}"
                                    :headers {"accept" "application/edn"
                                              "content-type" "application/edn"
                                              "content-length" 18 }})))
        "Everything working test")))
