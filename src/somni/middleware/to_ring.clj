(ns somni.middleware.to-ring)

(defn- ring?
  [response]
  (and (map? response)
       (some #{:status :headers :body} (keys response))))

(defn- ->ring
  [response]
  (cond
   (ring? response) response
   (instance? Throwable response) (throw response)
   :else {:status 200, :body response}))

(defn wrap-response-as-ring
  "If handler returns a ring response or an exception, pass it through.
  Otherwise pack result in a ring response."
  [handler]
  (fn [request] (->ring (handler request))))
