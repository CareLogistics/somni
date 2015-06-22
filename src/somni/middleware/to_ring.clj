(ns somni.middleware.to-ring)

(defn- ring?
  [response]
  (and (map? response)
       (some #{:status :headers :body} (keys response))))

(defn- ->ring
  [response]
  (if (or (ring? response)
          (instance? Throwable response))
    response
    {:status 200, :body response}))

(defn wrap-->ring
  "If handler returns a ring response or an exception, pass it through.
  Otherwise pack result in a ring response."
  [handler]
  (fn [request] (->ring (handler request))))
