(ns somni.middleware.access-control
  (:require [somni.http.errors :refer [not-authenticated
                                       access-denied]]
            [somni.misc :refer [by-tag]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; authentication
(defmulti  request->identity
  "Returns identity that should be associated with the request.
  If there is no identity, returns nil.

  *anonymous* returns an example identity for anonymous requests."
  by-tag)

(defmethod request->identity :default [& _] nil)

(def ^:dynamic *anonymous* (fn [request] {:user :anon, :roles nil}))

(defmethod request->identity :none [_ request] (*anonymous* request))

(defn wrap-authentication
  "Authentication of request using request->identity multimethod"
  [handler auth-provider]

  {:pre [(ifn? handler)
         (keyword? auth-provider)]}

  (fn [request]
    (if-some [id (request->identity auth-provider request)]
      (-> request (assoc :identity id)
          handler (assoc :identity id))
      (not-authenticated request))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; authorization
(defmulti  request->roles
  "Returns set of roles associated with a request.  If there is no user
  or the user has no roles, request->roles should return nil."
  by-tag)

(defmethod request->roles :default [_ req] (get-in req [:identity :roles]))

(defn- acls* [acls]
  (into {} (for [[k v] acls] [k (set (map name v))])))

(defn- access-allowed?
  "Tests if there's an intersection"
  [allowed-roles users-roles]
  (some (comp allowed-roles name) users-roles))

(defn- wrap-authorization*
  [handler acls role-provider]

  {:pre [(seq acls)]}

  (fn [{:as request :keys [request-method]}]

    (let [allowed-roles (set (acls request-method))
          users-roles   (set (request->roles role-provider request))]

      (if (access-allowed? allowed-roles users-roles)
        (handler request)
        (access-denied request)))))

(defn wrap-authorization
  "ACL based authorization middleware that uses request->roles multimethod
  to extract a users roles from request.

  acls is a map from operation to valid roles, e.g.:
    {:get [roles that are valid], :put [other roles]}

  When no acls are provided for the handler, returns to access-denied for all
  requests."
  ([handler acls role-provider]

   {:pre [(ifn? handler)]}

   (let [acls (acls* acls)]
     (if (seq acls)
       (wrap-authorization* handler acls role-provider)
       access-denied)))

  ([handler acls] (wrap-authorization handler acls :default)))
