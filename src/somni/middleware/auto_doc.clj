(ns somni.middleware.auto-doc)

(defn wrap-options
  "
  Returns 200 with body that describes the resource.
  "
  [handler resource & [schemas]]

  {:pre [handler resource]}

  ;; TODO: make something pretty out of that resource definition
  (fn [{:as request :keys [request-method]}]
    (if (= request-method :options)
      {:status       200
       :content-type "application/text"
       :body         resource}
      (handler request))))
