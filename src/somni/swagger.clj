(ns somni.swagger
  (:require [ring.swagger.swagger2 :as rs]
            [somni.middleware.negotiator :refer (->clj)]
            [somni.middleware.bindings :refer (get-path-params)]
            [liberator.representation :refer (render-map-generic)]
            [schema.core :as s]))

(defn dispatch-values [method] (-> (methods method)
                                   (dissoc :default)
                                   (keys)))

(defn consumes [] (dispatch-values ->clj))

(defn produces [] (dispatch-values render-map-generic))

(def ops #{:get :put :post :delete :any})

(defn op->swagger
  [bindings
   {:as meta
    :keys [doc tag consumes produces schema]}]

  (cond-> {}
    doc            (assoc :summary doc)
    consumes       (assoc :consumes consumes)
    produces       (assoc :produces produces)
    (map? tag)     (assoc-in [:parameters :body] tag)
    schema         (assoc-in [:responses 200 :schema] schema)
    (seq bindings) (assoc-in [:parameters :path]
                             (zipmap bindings (repeat s/Str)))))

(defn resources->swagger
  [resources]

  {:pre [(coll? resources)
         (every? map? resources)
         (every? (comp string? :uri) resources)
         (every? #(some % ops) resources)]}

  {:swagger 2.0
   :produces (produces)
   :consumes (consumes)
   :paths
   (into {} (for [resource resources
                  op ops
                  :let [handler (resource op)
                        uri (:uri resource)]
                  :when handler
                  :when uri]
              [uri {op (op->swagger
                        (get-path-params uri)
                        (meta handler))}]))})

(defn swagger-api
  [resources]
  (let [json (-> (resources->swagger resources)
                 (rs/swagger-json))]
    (fn [_] json)))
