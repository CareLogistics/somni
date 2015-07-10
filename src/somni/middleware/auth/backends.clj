(ns somni.middleware.auth.backends
  (:require [buddy.auth.backends.token :as token]
            [buddy.auth.backends.session :as session]
            [buddy.auth.protocols :refer :all]))

;;
(defmulti get-authn-backend :type)

(defmethod get-authn-backend :default
  [{:keys [type]}]
  (if (nil? type)
    nil
    (throw (ex-info "Authentication type invalid" {:type type}))))

;; buddy backends
;;{:type :jwe, :secret "changeit"}
(defmethod get-authn-backend :jws [m] (token/jws-backend m))
;;{:type :jws, :secret "foowhoWoohoo"}
(defmethod get-authn-backend :jwe [m] (token/jwe-backend m))
;;{:type :session}
(defmethod get-authn-backend :session [m] (session/session-backend) m)

;; care-logistics backends
#_{:type :ssl, :ask-eric "or look at patient itinerary"}
#_{:type :chained
 :auth-backends [{:type :session}
                 {:type :ssl, :ask-eric "or ditto above"}
                 {"Any" "other" "backend" "config" "goes" "here"}]}
