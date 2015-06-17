(ns somni.stacker
  "Configures middleware based upon resource definition."
  (:require [somni.misc :refer :all]
            [clojure.pprint :as pp]))

(defn- response?
  "
  Determines if a fn response is a ring-response.
  "
  [response]
  (and (map? response)
       (some #{:status :headers :body} (keys response))))

(defn- ex->response
  [ex]
  (let [data    (ex-data ex)
        status  (or (:status data) 400)
        message (.getMessage ex)]

    {:status  status
     :headers data
     :body   [message]}))

(defn- ->response
  "
  If handler returns a ring response, pass it through.  Otherwise pack its
  result in the responses body.
  "
  [response]
  (cond
   (response? response)           response
   (instance? Throwable response) (ex->response response)
   :else {:body response}))

(defn wrap-middleware
  "
  Adds a seq of middlware to a handler.
  "
  [handler middleware]
  (reduce (fn [a m] (m a)) handler middleware))

(defn- wrap
  "Conditionally adds middleware logic based upon availability of options."
  [handler middleware options]
  (cond
   (true?      options) (middleware handler)
   (non-empty? options) (middleware handler options) ;TODO: is this a relic?
   options              (middleware handler options)
   :else handler))

(defn- get-sec-fn
  ;; TODO: convert this to multi-method with default being a reject
  "
  Matches the security handler to the resource.  If it cannot find a match
  it will deny all access to the resource.
  "
  [sec-handlers resource]

  (let [sec-req (:security resource)
        sec-fn  (and sec-req (get sec-handlers sec-req))]

    (when sec-req
      (or sec-fn
          (do
             (pp/write {:error   "Missing Security Handler"
                        :sec-fn  (:security resource)
                        :uris    (:uris resource)
                        :message "All access will be denied."})
             (constantly nil))))))

(defn- gen-trace-id
  "
  Simple, roughly ordered, fast trace id generator.
  "
  []
  (java.util.UUID.
   (System/nanoTime)
   (* (rand-int (Integer/MAX_VALUE))
      (rand-int (Integer/MAX_VALUE)))))

(defn- self-described-handler
  "
  Merges the resource definition from the meta data attached to a handler
  function.
  "
  [{:as resource, r-ops :ops} handler]

  (let [handler-meta              (meta handler)
        {:keys [schema ops desc]} (:tag handler-meta)
        deps (seq (map (comp keyword name)
                       (drop 1 (first (:arglists handler-meta)))))]

    (-> resource
        (merge (when desc   {:desc   desc}))
        (merge (when schema {:schema schema}))
        (merge (when deps   {:deps   deps}))
        (merge (when ops    {:ops (if r-ops
                                    (filter (set ops) r-ops)
                                    ops)})))))

(defn- assoc-trace
  [ring-map trace-id]
  (assoc-in ring-map [:headers :trace-id] trace-id))

#_
(defn- stack-middleware
  "
  Builds a resource's request & response pipeline as documented in
  make-handler.
  "
  [resource handler {:as    options
                     :keys [sec-handlers, media-handlers, deps,
                            on-request, on-response,
                            schemas, dev-mode]}]

  {:pre [resource handler]}

  (let [resource (self-described-handler resource handler)

        mhs (and media-handlers (concat media-handlers builtin-media-handlers))

        request-fn
        (->
         handler

         ;; Inject dependencies
         (wrap wrap-deps (seq (map (or deps {}) (:deps resource))))

         ;; Add custom request middleware
         (wrap wrap-middleware on-request)

         ;; Request validation middleware
         (wrap wrap-schema-validation (or (get schemas (:schema resource))
                                          (:schema resource)))

         ;; Negotiation middleware
         (wrap wrap-deserialization     mhs)
         (wrap wrap-content-negotiation mhs)
         (wrap wrap-supported-methods (:ops resource))

         ;; Discovery middleware
         (wrap-options resource schemas)

         ;; Security middleware
         (wrap wrap-access-control (filter (comp request-methods key) resource))
         (wrap wrap-authentication (get-sec-fn sec-handlers resource)))

        response-fn
        (->
         identity

         ;; Add custom response middleware
         (wrap wrap-middleware on-response)

         ;; Negotiation response middleware
         (wrap wrap-serialization mhs))]

    ^{:resource-definition resource}
    (fn [{:as request :keys [trace-id]}]
      (let [trace-id (or trace-id (gen-trace-id))]

        (-> request
            (assoc-trace trace-id)
            (request-fn)
            (->response)
            (assoc-trace trace-id)
            (response-fn)
            (assoc-trace trace-id))))))

#_
(defn- build-resources
  "
  Collects detailed information required for construction of each resource.

  resources is the authoritative source for all uris, security, ops & roles

  handlers is the authoritative source for schema, deps & can further
  restrict available ops beyon definition from resources.

  options contains maps that will be used by resources to lookup sec-handlers,
  deps, on-request, on-response, on-missing & on-error handlers.  Dependencies
  required from handlers will also be resolved through options.
  "
  [resources handlers options]

  (for [{:as rsc, :keys [uris handler]} resources
        :let [hfn (handlers handler)]]

    ;; TODO: if we don't find a resource in handlers, resolve from callers ns
    (do
      (assert (ifn? hfn) (str "Handler function not defined for " rsc))

      {:uris      uris
       :resource (if (ifn? hfn)
                    (stack-middleware rsc hfn options)
                    server-error)})))
