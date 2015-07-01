(ns somni.middleware.negotiator
  (:require [clojure.data.json :as json]
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
(defmethod ->clj app-json [repr body] (json/read-str body))

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
