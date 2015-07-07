(ns somni.middleware.auth
  (:require [somni.middleware.auth.backends :as backends]
            [somni.http.errors :refer [not-authenticated]]
            [buddy.auth.protocols :as buddy-proto]))

(defn- authenticate
  "..."
  [backend request]
  (let [data (buddy-proto/parse backend request)]
    (if (nil? data)
      nil
      (buddy-proto/authenticate backend request data))))

(defn wrap-authentication
  "..."
  [handler backend]
  (fn [request]
    (if (nil? backend)
      (handler request))
      (if-some [id (authenticate backend request)]
         (-> request (assoc :identity id)
             handler (assoc :identity id))
         (not-authenticated request))))


