;;; Copyright (c) Care Logistics, inc. All rights reserved.
;;; The use and distribution terms for this software are covered by the
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.html at the root of this
;;; distribution.
;;; By using this software in any fashion, you are agreeing to be bound by
;;; the terms of this license.
;;; You must not remove this notice, or any other, from this software.

(ns somni.middleware.exceptions
  (:require [somni.http.errors :refer [server-error]]))

(def ^:dynamic *ignored-packages* #"^(clojure|java|immutant|io|ring|org).*")

(defn ex-details
  "Recursively converts a Java Throwable to clojure map with :exception,
  :message, :stackTrace & :cause"
  [^Throwable e]

  (let [details {:exception  (type e)
                 :message    (.getMessage e)
                 :stackTrace (remove #(re-matches *ignored-packages* %)
                                     (map str (.getStackTrace e)))}]

    (if-some [cause (.getCause e)]
      (assoc details :cause (ex-details cause))
      details)))

(defn pprint-ser [expr] (with-out-str
                          (clojure.pprint/pprint
                           expr)))

(require 'ring.util.request)

(def ^:private request-keys #{:server-port
                              :server-name
                              :remote-addr
                              :uri
                              :query-string
                              :scheme
                              :request-method
                              :content-type
                              :content-length
                              :character-encoding
                              :ssl-client-cert
                              :headers
                              :body})

(defn valid-header?
  [header]
  (every? string? header))

(defn validate-headers
  [headers]
  (into {} (filter valid-header? headers)))

(defn wrap-uncaught-exceptions
  ([next-fn on-error] (wrap-uncaught-exceptions next-fn on-error identity))

  ([next-fn on-error serialization-fn]

   {:pre [(every? ifn? [next-fn on-error serialization-fn])]}

   (fn [req]
     (let [resp (try (next-fn req) (catch Exception e e))]
       (if (instance? Throwable resp)
         (let [data (ex-data resp)
               body (:body data {:request (select-keys req request-keys)
                                 :details (ex-details resp)})]
           (on-error (-> data
                         (assoc :body (serialization-fn body))
                         (update-in [:headers] validate-headers))))
         resp)))))

