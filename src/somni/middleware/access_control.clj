(ns somni.middleware.access-control
  (:require [somni.http.errors :refer [not-authenticated
                                       access-denied]]
            [somni.misc :refer [by-tag]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; authentication
(defmulti request->identity by-tag)

(defmethod request->identity :default [& _] nil)

(defn wrap-authentication
  "..."
  ([handler auth-provider deps]

   {:pre [(ifn? handler)
          (or (get-method request->identity auth-provider)
              (get deps auth-provider))]}

   (let [authenticate (if-some [m ((methods request->identity) auth-provider)]
                        (partial m auth-provider)
                        (get deps auth-provider))]

     (fn [request]
       (if-some [id (authenticate request)]
         (-> request (assoc :identity id)
             handler (assoc :identity id))
         (not-authenticated request)))))

  ([handler auth-provider]
   (wrap-authentication handler auth-provider {})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; authorization
(defn- get-roles [req] (get-in req [:identity :roles]))

(defn- wrap-authorization*
  [handler acls]
  (fn [{:as request :keys [request-method uri]}]
    (let [users-roles (get-roles request)]
      (if (some acls users-roles)
        (handler request)
        (access-denied request)))))

(defn wrap-authorization
  "Role based authorization.  Expects roles information in ring request
  {:identity {:roles [s/Keywords] ...} ...}."
  ([handler acls]

   {:pre [(ifn? handler)]}

   (let [acls (set (map (comp keyword name) acls))]
     (if (seq acls)
       (wrap-authorization* handler acls)
       access-denied))))
