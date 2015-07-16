(ns somni.middleware.tracing)

(def ^:dynamic ^String *somni-trace-id* "Somni-Trace-Id")

(defn- gen-trace-id [] (java.util.UUID. (System/nanoTime) (System/nanoTime)))

(defn- assoc-trace
  [r trace-id]
  (assoc-in r [:headers *somni-trace-id*] trace-id))

(defn wrap-trace
  [handler]
  (fn [request]
    (let [trace-id (or (get-in request [:headers *somni-trace-id*])
                       (gen-trace-id))]
      (-> request (assoc-trace trace-id)
          handler (assoc-trace trace-id)))))
