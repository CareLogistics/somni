(ns somni.http.errors
  (:require [clojure.pprint :as pp]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Client-side errors

(defn malformed-request
  [{:as r :keys [dev-mode]}]
  (when dev-mode (pp/write r))
  {:status 400, :body "Malformed request"})

(defn not-authenticated  [_] {:status 401, :body "Authentication required"})
(defn access-denied      [_] {:status 403, :body "Access denied"})
(defn not-found          [_] {:status 404, :body "Not found"})
(defn unsupported-method [_] {:status 405, :body "Unsupported HTTP method"})
(defn not-acceptable     [_] {:status 406, :body "Not Acceptable"})
(defn unsupported-media  [_] {:status 415, :body "Unsupported content-type"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server-side errors

(defn- server-error? [status] (and (number? status)
                                   (>= status 500)
                                   (<  status 600)
                                   status))

(defn server-error
  "
  This is a general purpose server error.  It includes r either in console
  *out* or in the body of the http response.  If there is a server error
  :status set in r, it will be the :status used in the response.
  "
  [{:as r :keys [status]} & [dev-mode]]

  {:status (or (server-error? status) 500)
   :body (if (or dev-mode (:dev-mode r))
           (pp/write r :stream nil)
           (do (pp/write r)
               "Internal server error"))})
