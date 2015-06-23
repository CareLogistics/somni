(ns somni.stacker
  "Configures middleware based upon resource definition."
  (:require [somni.misc :refer :all]
            [clojure.pprint :as pp]
            [schema.core :as s]
            [somni.middleware.injectors :refer [inject-deps-into-request]]
            [somni.middleware.validation :refer [wrap-request-validation]]
            [somni.middleware.access-control :refer :all]
            [somni.middleware.auto-doc :refer [wrap-options]]
            [somni.middleware.negotiator :refer [wrap-content-negotiation]]
            [somni.middleware.bindings :refer [attach-bindings-to-request-params]]
            [somni.middleware.to-ring :refer [wrap-response-as-ring]]))

(def ops #{:get :put :post :delete})
(def ops-schema (apply s/enum ops))
(def authorization-schema {s/Keyword [ops-schema]})
(def resource-schema
  {:uri                                 s/Str
   ops-schema                           s/Symbol
   (s/optional-key :doc)                s/Str
   (s/optional-key :authentication)     s/Keyword
   (s/optional-key :disabled-methods) [ops-schema]
   (s/optional-key :authorization)      authorization-schema})

(def tags-schema
  {(s/optional-key :schema)   {s/Any s/Any}
   (s/optional-key :consumes) [s/Str]
   (s/optional-key :produces) [s/Str]
   s/Any s/Any})

(def merged-tags #{:schema :consumes :produces})
(def merged-meta #{:doc :arglists})
(def exclude-from-docs #{:disabled-methods :authentication :authorization})

(defn describe-resource
  [resource]

  ;; validate resource def
  (s/validate resource-schema resource)

  (let [handlers (for [op ops
                       :let [handler (resolve (resource op))
                             h-meta (meta handler)]
                       :when handler
                       :when (not ((set (:disabled-methods resource)) op))]

                   [op (merge
                        (into {} (filter (comp merged-tags key) (:tag h-meta)))
                        (into {} (filter (comp merged-meta key) h-meta))
                        {:handler handler})])]

    (reduce
     (fn [rd [op desc]] (assoc rd op desc))
     (apply dissoc resource ops)
     handlers)))

(defn description->roles
  [{:keys [authorization]}]
  (reduce
   (fn [a [op role]] (update-in a [op] (fnil conj #{}) role))
   {}
   (for [[role ops] authorization, op ops] [op role])))

(defn- gen-trace-id [] (java.util.UUID. (System/nanoTime) (System/nanoTime)))

(def ^:dynamic ^String *somni-trace-id* "Somni-Trace-Id")

(defn- trace-request
  [handler trace-id]
  (fn [req] (handler (assoc-in req [:headers *somni-trace-id*] trace-id))))

(defn- trace-response
  [handler trace-id]
  (fn [req] (assoc-in (handler req) [:headers *somni-trace-id*] trace-id)))

(defn wrap
  ([handler middleware options]
   (cond
    (non-empty? options) (middleware handler options)
    options              (middleware handler options)
    :else handler))
  ([handler middleware] (wrap handler middleware true)))

(defn wrap-middlewares [handler user-middlewares]
  (reduce wrap handler user-middlewares))

(defn stack-middleware
  ([resource-desc op deps user-middlewares]

   {:pre [resource-desc
          (keyword? op)
          (ops op)
          (or (nil? deps)
              (map? deps))]}

   (when-let [handler (get resource-desc op)]

     (let [trace-id (gen-trace-id)]

       (-> handler
           (inject-deps-into-request deps)
           (attach-bindings-to-request-params (:uri resource-desc))

           (wrap-response-as-ring)
           (trace-response trace-id)

           (trace-request trace-id)
           (wrap-middlewares user-middlewares) ; request & response
           (trace-response trace-id)

           (wrap wrap-request-validation (get-in resource-desc [op :schema]))

           (wrap-content-negotiation (resource-desc op)) ; request & response

           ;; below is request middleware
           (wrap-options (remove exclude-from-docs resource-desc))
           (wrap wrap-authorization (op (description->roles resource-desc)))
           (wrap wrap-authentication (:authentication resource-desc))
           (trace-request trace-id))))))
