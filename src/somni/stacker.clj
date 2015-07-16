(ns somni.stacker
  "Configures middleware based upon resource definition."
  (:require [somni.misc :refer :all]
            [somni.middleware.injectors :refer [inject-deps]]
            [somni.middleware.validation :refer [wrap-request-validation]]
            [somni.middleware.extractions :refer [wrap-extractions]]
            [somni.middleware.negotiator :refer [wrap-negotiator]]
            [somni.middleware.bindings :refer [attach-bindings]]
            [somni.middleware.to-ring :refer [wrap-response-as-ring]]
            [somni.middleware.exceptions :refer [wrap-uncaught-exceptions]]
            [somni.middleware.tracing :refer [wrap-trace]]))

(def ops #{:get :put :post :delete :any})

(def merged-tags #{:schema :consumes :produces})

(def merged-meta #{:doc :arglists})

(defn- describe-resource
  [resource]

  {:pre [(map? resource)
         (string? (:uri resource))
         (some resource ops)]}

  (let [handlers (for [op ops
                       :let [handler (resource op)
                             h-meta (meta handler)]
                       :when handler]

                   [op (merge
                        (into {} (filter (comp merged-tags key) (:tag h-meta)))
                        (into {} (filter (comp merged-meta key) h-meta))
                        {:handler handler})])]

    (reduce (fn [rd [op desc]] (assoc rd op desc))
            (apply dissoc resource ops)
            handlers)))

(defn- wrap-middleware
  [handler user-middleware]
  (reduce #(%2 %1) handler user-middleware))

(defn- config-stacker
  [resource-desc op deps user-middleware on-error]
  {:handler (get-in resource-desc [op :handler])
   :uri     (get-in resource-desc [:uri])
   :op       op
   :extract (#{:any :get} op)
   :mw       user-middleware
   :schema  (get-in resource-desc [op :schema])
   :conneg  (not-any? (get-in resource-desc [op]) [:produces :consumes])
   :on-error on-error
   :deps     deps})

(defn- stack-middleware
  [{:keys [handler deps uri extract mw schema conneg on-error auth acls]}]

  {:pre [handler uri]}

  (cond-> handler
          :always      (inject-deps deps)
          :always      (attach-bindings uri)
          extract      (wrap-extractions uri)
          :always      (wrap-response-as-ring)
          (seq mw)     (wrap-middleware mw)
          (seq schema) (wrap-request-validation schema)
          :always      (wrap-uncaught-exceptions on-error) ; serializable errors
          conneg       (wrap-negotiator)
          :always      (wrap-uncaught-exceptions on-error) ; unserializable errors
          :always      (wrap-trace)))

(def ^:private configure-handler (comp stack-middleware config-stacker))

(defn stack
  ([resource deps & {:as options :keys [user-middleware on-error]}]

   (let [resource-desc (describe-resource resource)]

     (for [op ops :when (op resource-desc)]
       [op
        (:uri resource-desc)
        (configure-handler resource-desc op deps user-middleware on-error)])))

  ([resource] (stack resource {} [])))
