(ns somni.swagger
  (:require [camel-snake-kebab.core :refer (->camelCase ->PascalCase)]
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

(defn- bindings->swagger
  [bindings]
  (zipmap (map ->camelCase bindings) (repeat s/Str)))

(defn- ->swagger
  [{:keys [doc tag response consumes produces schema bindings]}]

  (cond-> {}
    doc                 (assoc :summary doc)
    consumes            (assoc :consumes consumes)
    produces            (assoc :produces produces)
    (seq bindings)      (assoc-in [:parameters :path] (bindings->swagger bindings))
    (schema? response)  (assoc-in [:responses 200 :schema] response)
    (schema? tag)       (assoc-in [:responses 200 :schema] tag)
    (schema? schema)    (assoc-in [:parameters :body] schema)))

(defn resource->swagger
  [{:as resource :keys [uri]}]

  (let [uri (str/replace uri #"\$" ":")]

    (for [op ops
          :let [handler (op resource)]
          :when (var? handler)]

      [[uri op] (->swagger (assoc (merge resource (meta handler))
                                  :bindings (get-path-params uri)))])))

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

(defn- recase
  [x]
  (if (or (string? x)
          (instance? clojure.lang.Named x))
    (->camelCase x)
    x))

(defn- definitions-required?
  [x]
  (and (instance? clojure.lang.MapEntry x)
       (= :required (first x))
       (coll? (second x))))

(defn- ref?
  [x]
  (and (instance? clojure.lang.MapEntry x)
       (= :$ref (first x))))

(defn- fix-def-ref
  [x]
  (let [z (recase (last (str/split x #"/")))]
    (format "#/definitions/%s" z)))

(defn- fix-casing
  [sw]
  (clojure.walk/prewalk
   (fn [arg]
     (cond
       (definitions-required? arg)
       [:required (map ->camelCase (second arg))]

       (ref? arg)
       [:$ref (fix-def-ref (second arg))]

       (instance? clojure.lang.MapEntry arg)
       [(recase (first arg)) (second arg)]

       :else arg))
   sw))

(defn swagger-json
  [resources]
  (render-map-generic (fix-casing
                       (rs/swagger-json (resources->swagger resources)))
                      {:representation
                       {:media-type "application/json"
                        :charset "UTF-8"}}))

(defn swagger-api
  [resources]
  (let [json (swagger-json resources)]
    (fn [_] {:status 200,
            :body  json                 ; cached indefinitely
            :headers {"Content-Type" "application/json"}})))
