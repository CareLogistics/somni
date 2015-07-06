(ns somni.http.errors
  (:require [clojure.pprint :refer [pprint]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Client-side errors

(defn malformed-request  [_] {:status 400, :body "Malformed request"})
(defn not-authenticated  [_] {:status 401, :body "Authentication required"})
(defn access-denied      [_] {:status 403, :body "Access denied"})
(defn not-found          [_] {:status 404, :body "Not found"})
(defn unsupported-method [_] {:status 405, :body "Unsupported HTTP method"})
(defn not-acceptable     [_] {:status 406, :body "Not Acceptable"})
(defn unsupported-media  [_] {:status 415, :body "Unsupported Content-Type"})

(defn client-error? [status] (and (number? status)
                                   (>= status 400)
                                   (<  status 500)
                                   status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server-side errors

(defn server-error? [status] (and (number? status)
                                   (>= status 500)
                                   (<  status 600)
                                   status))

(defn server-error
  "..."
  ([{:as resp :keys [status]} dev-mode]
   (let [status (or (server-error? status) 500)
         resp (assoc resp :status status)]

     (if dev-mode
       resp
       (do (pprint resp)
           {:status status,
            :body {:error "Internal server error"}}))))

  ([resp] (server-error resp nil)))

(defn http-error? [status] (or (client-error? status)
                               (server-error? status)))
