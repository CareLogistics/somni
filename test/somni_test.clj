(ns somni-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [somni :refer :all]))

(defn hello [r name body db]
  (prn r)
  (cond
   body body
   name (db "Hello " name)
   :else "Hello from Somni"))

(def User {:username s/Str})

(defn ^{:schema User} new-user
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
  {:user (assoc identity :uid (db uid))})

(def sample-resources
  [
   {:uri "hello/:name"
    :doc "Hello Somni example"
    :any #'hello}

   {:uri    "user/:uid/?"
    :doc    "User management"
    :put    #'new-user
    :delete #'delete-user
    :get    #'get-user}

   {:uri   "user"
    :doc   "Add a user, with server generated uid"
    :post  #'new-user}])

(deftest somni-tests
  (let [somni-handler (build sample-resources {:db str})]

    (is (= "Hello Philip J. Fry"
           (:body (somni-handler {:uri "hello"
                                  :request-method :get
                                  :params {:name "Philip J. Fry"}}))
           (:body (somni-handler {:uri "hello/Philip J. Fry"
                                  :request-method :get})))
        "Basic handler test")

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
