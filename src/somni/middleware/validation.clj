(ns somni.middleware.validation
  (:require [schema.core :as s]
            [somni.http.errors :refer [malformed-request]]))

(defn wrap-schema-validation
  "
  Returns 400 if the body of a post, put or patch does not match the
  schema.
  "
  [handler schema]

  {:pre [handler schema]}

  (fn [{:as request :keys [body request-method]}]
    (if-some [errors (and (#{:post, :put, :patch} request-method)
                          (s/check schema body))]
      (assoc (malformed-request request) :errors errors)
      (handler request))))
