(ns somni.stacker
  "Configures middleware based upon resource definition."
  (:require [somni.misc :refer :all]
            [clojure.pprint :as pp]
            [schema.core :as s]))

(def ops #{:get :put :post :delete})
(def ops-schema (apply s/enum ops))
(def roles-schema {s/Keyword [ops-schema]})
(def resource-def-schema {:uris                     [s/Str]
                          ops-schema                 s/Symbol
                          (s/optional-key :doc)      s/Str
                          (s/optional-key :security) s/Keyword
                          (s/optional-key :ops)     [ops-schema]
                          (s/optional-key :roles)    roles-schema})

(def tags-schema {(s/optional-key :schema)   {s/Any s/Any}
                  (s/optional-key :consumes) [s/Str]
                  (s/optional-key :produces) [s/Str]
                  s/Any s/Any})

(def merged-tags #{:schema :consumes :produces})
(def merged-meta #{:doc :arglists})

(defn describe-resource
  [resource-def]

  ;; validate resource def
  (s/validate resource-def-schema resource-def)

  (let [handlers (for [op ops
                       :let [handler (resolve (resource-def op))
                             h-meta (meta handler)]
                       :when handler]

                   [op (merge
                        (into {} (filter (comp merged-tags key) (:tag h-meta)))
                        (into {} (filter (comp merged-meta key) h-meta))
                        {:handler handler})])]

    (reduce
     (fn [rd [op desc]] (assoc rd op desc))
     resource-def
     handlers)))










































(defn wrap-middleware [h mws] (reduce (fn [a m] (m a)) h mws))

(defn- gen-trace-id [] (java.util.UUID. (System/nanoTime) (System/nanoTime)))

(defn- wrap
  [handler middleware options]
  (cond
   (true?      options) (middleware handler)
   (non-empty? options) (middleware handler options) ;TODO: is this a relic?
   options              (middleware handler options)
   :else handler))



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
                     :keys [deps,
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
