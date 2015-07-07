(ns somni.middleware.auth.backends
  (:require [buddy.auth.backends.token :as token]
            [buddy.auth.backends.session :as session]
            [buddy.auth.protocols :refer :all]))

;;
(defmulti get-auth-backend :type)

;; buddy backends
;;{:type :jwe, :secret "changeit"}
(defmethod get-auth-backend :jws [m] (token/jws-backend m))
;;{:type :jws, :secret "foowhoWoohoo"}
(defmethod get-auth-backend :jwe [m] (token/jwe-backend m))
;;{:type :session}
(defmethod get-auth-backend :session [m] (session/session-backend) m)

;; care-logistics backends
#_{:type :ssl, :ask-eric "or look at patient itinerary"}
#_{:type :chained
 :auth-backends [{:type :session}
                 {:type :ssl, :ask-eric "or ditto above"}
                 {"Any" "other" "backend" "config" "goes" "here"}]}
