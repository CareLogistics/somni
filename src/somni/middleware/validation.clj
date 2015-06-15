(ns somni.middleware.validation)

(defn wrap-schema-validation
  "
  Returns 400 if the body of a pust, put or patch does not match the
  schema.
  "
  [handler schema]

  {:pre [handler schema]}

  (fn [{:as request :keys [body request-method]}]
    (if-some [errors (and (#{:post, :put, :patch} request-method)
                          (s/check schema body))]
      (malformed-request (assoc request :errors errors))
      (handler request))))
