(ns somni.middleware.auth
  (:require [buddy.auth.accessrules :refer [success error]]
            [buddy.auth.protocols :as buddy-proto]
            [buddy.auth :refer [authenticated?]]
            [somni.http.errors :refer [not-authenticated]]
            [somni.middleware.auth.backends :as backends]))

(defn- authenticate
  "..."
  [backend request]
  (when-some [data (buddy-proto/parse backend request)]
    (buddy-proto/authenticate backend request data)))

(defn- wrap-authentication*
  [handler backend]
  (fn [request]
    (let [id (authenticate backend request)]
      (-> request (assoc :identity id)
          handler (assoc :identity id)))))

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
