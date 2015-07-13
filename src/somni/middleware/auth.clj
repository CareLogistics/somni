(ns somni.middleware.auth
  (:require [buddy.auth.accessrules :refer [success error]]
            [buddy.auth.protocols :as buddy-proto]
            [buddy.auth :refer [authenticated?]]
            [somni.http.errors :refer [not-authenticated
                                       access-denied]]))

#_(defn authenticate [a b] nil)
(defn- authenticate
  "..."
  [backend request]
  (let [request* (when-some [data (buddy-proto/parse backend request)]
                            (buddy-proto/authenticate backend request data))]
    (if-not (nil? request*)
      (request* :identity)
      nil)))

(defn- wrap-authentication*
  [handler backend]
  (fn [request]
    (let [id (authenticate backend request)]
      (if-not (nil? id)
        (-> request (assoc :identity id)
            handler (assoc :identity id))
        (not-authenticated request)))))

(defn wrap-authentication
  "..."
  [handler backend]
  (if backend
    (wrap-authentication* handler backend)
    handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defn grant-to
  "..."
  [roles]
  (let [roles (set roles)]
    (fn [{:as request :keys [identity]}]
      (if (and (authenticated? request)
               (some roles (:roles identity)))
        true
        (error "Not authorized")))))
