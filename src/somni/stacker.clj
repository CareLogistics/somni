(ns somni.stacker
  "Configures middleware based upon resource definition."
  (:require [somni.misc :refer :all]
            [clojure.pprint :as pp]
            [schema.core :as s]
            [somni.middleware.injectors :refer [inject-deps-into-handler]]
            [somni.middleware.validation :refer [wrap-request-validation]]
            [somni.middleware.access-control :refer :all]
            [somni.middleware.auto-doc :refer [wrap-options]]
            [somni.middleware.negotiator :refer [wrap-negotiator]]
            [somni.middleware.bindings :refer [attach-bindings-to-request-params]]
            [somni.middleware.to-ring :refer [wrap-response-as-ring]]))

(def ops #{:get :put :post :delete :any})
(def ops-schema (apply s/enum ops))
(def authorization-schema {ops-schema #{s/Keyword}})
(def resource-schema
  {:uri                                s/Str
   ops-schema                          s/Any
   (s/optional-key :doc)               s/Str
   (s/optional-key :authentication)    s/Keyword
   (s/optional-key :disabled-methods) [ops-schema]
   (s/optional-key :authorization)     authorization-schema})

(def tags-schema
  {(s/optional-key :schema)   {s/Any s/Any}
   (s/optional-key :consumes) [s/Str]
   (s/optional-key :produces) [s/Str]
   s/Any s/Any})

(def merged-tags #{:schema :consumes :produces})
(def merged-meta #{:doc :arglists})
(def exclude-from-docs #{:disabled-methods :authentication :authorization})

(defn- describe-resource
  [resource]

  ;; validate resource def
  (s/validate resource-schema resource)

  (let [handlers (for [op ops
                       :let [handler (resource op)
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

(defn- gen-trace-id [] (java.util.UUID. (System/nanoTime) (System/nanoTime)))

(def ^:dynamic ^String *somni-trace-id* "Somni-Trace-Id")

(defn- assoc-trace
  [r trace-id]
  (assoc-in r [:headers *somni-trace-id*] trace-id))

(defn- wrap-middleware
  [handler user-middleware]
  (reduce #(%2 %1) handler user-middleware))

(defn wrap-trace
  [handler]
  (fn [request]
     (let [trace-id (or (get-in request [:headers *somni-trace-id*])
                         (gen-trace-id))]
        (-> request (assoc-trace trace-id)
            handler (assoc-trace trace-id)))))

(defn- config-stacker
  [resource-desc op deps user-middleware]
  {:handler (get-in resource-desc [op :handler])
   :uri     (get-in resource-desc [:uri])
   :op       op
   :mw       user-middleware
   :schema   (get-in resource-desc [op :schema])
   :conneg  (empty? (filter #{:produces :consumes} (get-in resource-desc [op])))
   :deps    deps
   :auth    (get-in resource-desc [:authentication])
   :acls    (get-in resource-desc [:authorization op])})

(defn- stack-middleware
  [{:keys [handler deps uri mw schema conneg auth acls]}]

  {:pre [handler uri]}

  (cond-> handler
          :always      (inject-deps-into-handler deps)
          :always      (attach-bindings-to-request-params uri)
          :always      (wrap-response-as-ring)
          (seq mw)     (wrap-middleware mw)
          (seq schema) (wrap-request-validation schema)
          conneg       (wrap-negotiator)
          (seq acls)   (wrap-authorization acls)
          auth         (wrap-authentication auth deps)
          :always      (wrap-trace)))

(def ^:private configure-handler (comp stack-middleware config-stacker))

(defn stack
  ([resource deps user-middleware]
   ;; [op path handaaaaler-fn] where path is uri + op
   (let [resource-desc (describe-resource resource)]

     (for [op ops :when (op resource-desc)]
       [op
        (:uri resource-desc)
        (configure-handler resource-desc op deps user-middleware)])))

  ([resource] (stack resource {} [])))
