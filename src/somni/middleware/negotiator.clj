;;; Copyright (c) Care Logistics, inc. All rights reserved.
;;; The use and distribution terms for this software are covered by the
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.html at the root of this
;;; distribution.
;;; By using this software in any fashion, you are agreeing to be bound by
;;; the terms of this license.
;;; You must not remove this notice, or any other, from this software.

(ns somni.middleware.negotiator
  (:require [camel-snake-kebab.core :refer :all]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [liberator.representation :refer :all]
            [ring.util.request :refer [body-string content-length]]
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
  [repr]
  (when-some [des ((methods ->clj) (:media-type repr))]
    (partial des repr)))

(defn- get-content
  [request]
  (when (pos? (or (content-length request) 0))
    (body-string request)))

(defn wrap-supported-media
  ([handler]
   (wrap-supported-media handler #{}))

  ([handler consumes]
   {:pre [(set? consumes)]}
   (fn [request]
     (let [body       (get-content request)
           repr       (when body (content-type request))
           consumable (when repr (consumes (:media-type repr)))
           des        (when repr (when-not consumable (deserializable? repr)))]
       (cond
         (nil? body) (handler (assoc request :body nil))
         consumable  (handler request)
         des         (handler (assoc request :body (des body)))
         :else       (unsupported-media request))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialization
(defn- write-generic [x ^java.io.PrintWriter out]
  (if (.isArray (class x))
    (json/-write (seq x) out)
    (json/-write (pr-str x) out)))

(extend java.lang.Object
  json/JSONWriter
  {:-write write-generic})

(extend-type java.util.Map
  Representation
  (as-response [this context]
    (as-response (render-map-generic this context) context)))

(defn- key-fn
  [x]
  (cond
   (instance? clojure.lang.Named x) (name x)
   (nil? x) (throw (Exception. "Serialized object properties may not be nil"))
   :else (str x)))

(defn json-write
  [data]
  (json/write-str
   data
   :key-fn (comp ->camelCase key-fn)))

(defmethod render-map-generic "application/json"
  [data context]
  (json-write data))

(defmethod render-seq-generic "application/json"
  [data context]
  (json-write data))

(def ^:dynamic *wildcard-media-types* #{"*" "*/*" "text/*" "application/*"})

(def ^:dynamic *default-media-type* app-json)

(defn- wildcards->default-media-type
  [representation]
  (if (*wildcard-media-types* (:media-type representation))
    (assoc representation :media-type *default-media-type*)
    representation))

(defn- renderable?
  "Using librarator's representation namespace for response rendering."
  [repr]
  ((methods render-map-generic) (:media-type repr)))

(defn- matching-serializers
  [accepts]
  (for [repr accepts
        :let [repr (wildcards->default-media-type repr)
              ser (renderable? repr)]
        :when ser]
    [repr #(ser % {:represenation repr})]))

(defn wrap-acceptable
  ([handler]
   (wrap-acceptable handler #{}))

  ([handler produces]
   {:pre [(set? produces)]}

   (fn [request]
     (let [acceptable   (accept request)
           [best-match] (matching-serializers acceptable)
           [repr ser]   best-match
           producable   (some (comp produces :media-type) acceptable)]
       (cond
         producable (handler request)

         ser (let [resp (handler request)]
               (if (:body resp)
                 (assoc-in (update-in resp [:body] ser)
                           [:headers "Content-Type"]
                           (format "%s;charset=%s"
                                   (:media-type repr)
                                   (:charset repr "UTF-8")))
                 resp))

         :else (not-acceptable request))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; negotiation

(defn wrap-negotiator
  [handler & {:keys [consumes produces]
              :or {consumes #{}
                   produces #{}}}]
  (-> handler
      (wrap-supported-media consumes)
      (wrap-acceptable produces)))
