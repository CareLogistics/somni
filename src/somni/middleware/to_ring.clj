(ns somni.middleware.to-ring)

(defn- ring?
  [response]
  (and (map? response)
       (some #{:status :headers :body} (keys response))))

(defn- ->response
  [status body]
  {:status status :body body})

(defmulti  response-status
  (fn [{:as req :keys [request-method]} resp] request-method))

(defmethod response-status :get     [_ resp] (if resp 200 404))

(defmethod response-status :default [_ resp] (if resp 200 204))

(defn wrap-response-as-ring
  "If handler returns a ring response or an exception, pass it through.
  Otherwise pack result in a ring response."
  [handler]
  (fn [{:as req :keys [request-method]}]
    (let [resp (handler req)]
      (cond
        (ring? resp)                resp
        (instance? Throwable resp) (throw resp)
        :else                      (->response
                                    (response-status req resp)
                                    resp)))))
