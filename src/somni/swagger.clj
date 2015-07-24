(ns somni.swagger
  (:require [camel-snake-kebab.core :refer (->camelCase)]
            [clojure.string :as str]
            [liberator.representation :refer (render-map-generic)]
            [ring.swagger.swagger2 :as rs]
            [schema.core :as s]
            [somni.middleware.bindings :refer (get-path-params)]
            [somni.middleware.negotiator :refer (->clj)]))

(defn dispatch-values [method] (-> (methods method)
                                   (dissoc :default)
                                   (keys)))

(def ops #{:get :put :post :delete})

(defn schema? [x] (satisfies? s/Schema x))

(defn- meta->swagger
  [{:keys [doc tag response consumes produces schema]}]

  (cond-> {}
    doc                 (assoc :summary doc)
    consumes            (assoc :consumes consumes)
    produces            (assoc :produces produces)
    (schema? response)  (assoc-in [:responses 200 :schema] response)
    (schema? tag)       (assoc-in [:responses 200 :schema] tag)
    (schema? schema)    (assoc-in [:parameters :body] schema)))

(defn- bindings->swagger
  [bindings]
  (when (seq bindings)
    {:parameters {:path (zipmap (map ->camelCase bindings)
                                (repeat s/Str))}}))

(defn resource->swagger
  [{:as resource :keys [uri]}]

  (let [uri (str/replace uri #"\$" ":")]

    (for [op ops
          :let [handler (op resource)]
          :when (var? handler)]

      [[uri op] (merge (bindings->swagger (get-path-params uri))
                       (meta->swagger (merge resource
                                             (meta handler))))])))

(defn resources->swagger
  [resources]

  {:pre [(coll? resources)
         (every? map? resources)
         (every? (comp string? :uri) resources)
         (every? #(some % ops) resources)]}

  {:produces (dispatch-values render-map-generic)
   :consumes (dispatch-values ->clj)
   :paths (reduce
           (fn [a [ks swag]] (assoc-in a ks swag))
           {}
           (mapcat resource->swagger resources))})

(defn swagger-api
  [resources]
  (let [json (render-map-generic
              (-> (resources->swagger resources)
                  (rs/swagger-json))
              {:representation
               {:media-type "application/json"
                :charset "UTF-8"}})]

    (fn [_] {:status 200,
            :body  json                 ; cached indefinitely
            :headers {"Content-Type" "application/json"}})))
