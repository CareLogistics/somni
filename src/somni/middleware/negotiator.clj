(ns somni.middleware.negotiator
  (:require [camel-snake-kebab.core :refer :all]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [liberator.representation :refer :all]
            [ring.util.request :refer [body-string]]
            [somni.http.errors :refer [not-acceptable unsupported-media]]
            [somni.http.forms :refer [form-decode]]
            [somni.http.mime :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string constants
(def ^:const app-edn "application/edn")
(def ^:const app-clj "application/clojure")
(def ^:const app-json "application/json")
(def ^:const www-form "application/x-www-form-urlencoded")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; deserialization
(defmulti ->clj
  "Select a stream parser based upon the media-type defined by the request's
  content-type."
  (fn [repr body] (:media-type repr)))

(defmethod ->clj app-clj [repr body] (edn/read-string body))
(defmethod ->clj app-edn [repr body] (edn/read-string body))

(defmethod ->clj www-form [repr body] (form-decode body))

(defmethod ->clj app-json [repr body]
  (json/read-str body :key-fn (comp keyword ->kebab-case)))

(defn deserializable?
  [request]
  (let [repr (content-type request)]
    (when-some [des ((methods ->clj) (:media-type repr))]
      (partial des repr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialization
(defn renderable?
  "Using librarator's representation namespace for response rendering."
  [repr]
  ((methods render-map-generic) (:media-type repr)))

(def ^:dynamic *default-media-type* app-json)

(defn set-default-media-type!
  [media-type]
  (when (renderable? {:media-type media-type})
    (alter-var-root *default-media-type* (fn [_] media-type))))

(def ^:dynamic *json-naming-style* ->snake_case)

(def supported-name-styles
  {:snake ->snake_case :camel ->camelCase :kebab ->kebab-case :pascal ->PascalCase})

(defn set-json-case!
  [style]
  {:pre [(supported-name-styles style)]}
  (alter-var-root *json-naming-style*
                  (fn [_] (supported-name-styles style))))

(defn- key-fn
  [x]
  (cond
   (instance? clojure.lang.Named x) (name x)
   (nil? x) (throw (Exception. "Serialized object properties may not be nil"))
   :else (str x)))

(defn- write-generic [x ^java.io.PrintWriter out]
  (if (.isArray (class x))
    (json/-write (seq x) out)
    (json/-write (pr-str x) out)))

(extend java.lang.Object json/JSONWriter {:-write write-generic})

(extend-type java.util.Map
  Representation
  (as-response [this context]
    (as-response (render-map-generic this context) context)))

(defn- json-write [data] (json/write-str data :key-fn (comp *json-naming-style* key-fn)))

(defmethod render-map-generic "application/json" [data context] (json-write data))
(defmethod render-seq-generic "application/json" [data context] (json-write data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; functions that deal with ring request
(def ^:dynamic *wildcard-media-types* #{"*" "*/*" "text/*" "application/*"})

(defn- wildcards->default-media-type
  [representation]
  (if (*wildcard-media-types* (:media-type representation))
    (assoc representation :media-type *default-media-type*)
    representation))

(defn- best-representation
  [request]
  (first
   (for [repr (accept request)
         :let [repr (wildcards->default-media-type repr)
               ser (renderable? repr)]
         :when ser]
     repr)))

(defn- content?
  [request]
  (some-> (get-in request [:headers "content-length"])
          (#(Long. %))
          (pos?)))

(defn- negotiate
  [request]

  (let [payload? (content? request)
        des (when payload? (deserializable? request))
        repr (best-representation request)]

    (cond
     (and payload? (nil? des)) :unsupported
     (nil? repr)               :not-acceptable
     :else                     {:des des :repr repr})))

(defn- marshall
  [handler request {:as opts :keys [des repr]}]

  (let [body    (body-string request)
        entity  ((or des identity) body)
        request (assoc request :body entity)
        resp    (handler request)

        ;; serializes response
        lib (as-response (:body resp resp) {:representation repr})]

    ;; merge lib result with handler result
    (-> resp
        (assoc :body (:body lib))
        (update-in [:headers] merge (:headers lib)))))

(defn wrap-negotiator
  [handler]
  (fn [request]
    (let [negotiated (negotiate request)]
      (condp = negotiated
        :unsupported    (unsupported-media request)
        :not-acceptable (not-acceptable request)
        (marshall handler request negotiated)))))
