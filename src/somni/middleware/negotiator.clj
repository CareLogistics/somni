(ns somni.middleware.negotiator
  (:require [somni.http.errors :refer :all]
            [somni.http.forms :refer [form-decode]]
            [clojure.edn :as edn]
            [somni.http.mime :refer [parse-mime parse-accept]]
            [somni.misc :refer [by-tag has-method]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; marshalling interface
(defmulti ->clj
  "deserialize based upon content-type of request"
  by-tag)

(defmulti clj->
  "serialize based upon content-type chosen from accept header"
  by-tag)

(def deserializable? (partial has-method ->clj))
(def serializable?   (partial has-method clj->))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string constants
(def ^:const headers-content-type [:headers "Content-Type"])
(def ^:const headers-accept       [:headers "Accept"])
(def ^:const content-type-default "application/octet-stream")
(def ^:const content-type-any     "*/*")
(def ^:const content-type-edn     "application/edn")
(def ^:const content-type-clj     "application/clojure")
(def ^:const content-type-form    "application/x-www-form-urlencoded")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Marshalling implementations for clojure & edn
(defmethod clj-> content-type-any [_ body] (pr-str body))
(defmethod clj-> content-type-edn [_ body] (pr-str body))
(defmethod ->clj content-type-edn [_ body] (edn/read-string body))
(defmethod clj-> content-type-clj [_ body] (pr-str body))
(defmethod ->clj content-type-clj [_ body] (edn/read-string body))

;;; Other built-in marshallers
(defmethod ->clj content-type-form [_ body] (form-decode body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; functions that deal with ring request
(defn- realize-body
  "...consider implementing as delay...
  ...or as function on stream...
  ...or as future, promise or whatever works best..."
  ([{:as req :keys [body]} encoding]
   (if (string? body)
     req
     (update-in req [:body] slurp :encoding encoding)))

  ([req]
   (realize-body req nil)))

(defn- marshall-with [r f] (update-in r [:body] f))

(defn- set-content-type [resp mime]
  (let [mime (name mime)]
    (-> resp
        (assoc :content-type mime)
        (assoc-in headers-content-type mime))))

(defn- content-type
  [request]
  (some->> (or (:content-type request)
               (get-in request headers-content-type)
               content-type-default)
           parse-mime
           #(when (deserializable? (:mime %)) %)))

(defn- accept
  [request]
  (some->> (or (get-in request headers-accept)
               (get-in request [:headers "accept"]))
           parse-accept
           (filter (comp serializable? :mime))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the actual middleware
(defn wrap-content-negotiation
  [handler]

  (fn [{:as request :keys [body]}]

    #_
    (let [content-type (request->content-type request)
          accepts nil]

      (cond
       (and body (not des)) (unsupported-media request)

       (nil? ser) (not-acceptable request)

       :else (let [resp (-> request
                            (realize-body )
                            (marshall-with des)
                            handler)]
               (if (:body resp)
                 (-> resp
                     (marshall-with ser)
                     (assoc :content-type out-mime))
                 resp))))))
