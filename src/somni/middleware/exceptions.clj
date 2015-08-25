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

(defn ex-details
  "Recursively converts a Java Throwable to clojure map with :exception,
  :message, :stackTrace & :cause"
  [^Throwable e]

  (let [details {:exception  (type e)
                 :message    (.getMessage e)
                 :stackTrace (remove #(re-matches #"^(clojure|java).*" %)
                                     (map str (.getStackTrace e)))}]

    (if-some [cause (.getCause e)]
      (assoc details :cause (ex-details cause))
      details)))

(defn pprint-ser [expr] (with-out-str
                          (clojure.pprint/pprint
                           expr)))

(defn wrap-uncaught-exceptions
  ([next-fn on-error] (wrap-uncaught-exceptions next-fn on-error identity))

  ([next-fn on-error serialization-fn]

   {:pre [(every? ifn? [next-fn on-error serialization-fn])]}

   (fn [req]
     (let [resp (try (next-fn req) (catch Exception e e))]
       (if (instance? Throwable resp)
         (let [data (ex-data resp)
               body (:body data {:request req, :details (ex-details resp)})]
           (on-error (assoc data :body (serialization-fn body))))
         resp)))))
