(ns somni.middleware.auto-doc)

(defn wrap-options
  "
  Returns 200 with body that describes the resource.
  "
  [handler resource-desc]

  {:pre [handler resource-desc]}

  ;; TODO: make something pretty out of that resource definition
  (fn [{:as request :keys [request-method]}]
    (if (= request-method :options)
      {:status       200
       :content-type "application/text"
       :body         resource-desc}
      (handler request))))
