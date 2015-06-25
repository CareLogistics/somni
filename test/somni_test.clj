(ns somni-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [somni.middleware.access-control :refer [request->identity]]
            [somni :refer :all]))

(defn hello [name] (if name (str "Hello " name) "Hello from Somni"))

(def +new-user+ {:schema {:username s/Str}})

(defn ^+new-user+ new-user
  "Creates a new user with uid"
  ([uid body identity db]
   (db :delete {:username (:username body), :uid uid, :created-by identity}))

  ([body identity db]
   (new-user (java.util.UUID/randomUUID) body identity)))

(defn delete-user
  [uid identity db]
  (db :delete uid " deleted by " identity))

(defn get-user
  [uid identity db]
  (db :get uid " read by " identity))

(defmethod request->identity :trusting [_ request] (:identity request))

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
                    :put    #{:admin}}}

   {:uri   "user"
    :doc   "Add a user, with server generated uid"
    :post  #'new-user
    :authentication :trusting
    :authorization {:post #{:admin}}}])
