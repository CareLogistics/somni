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

(def ^:dynamic *default-mime-type* content-type-edn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Marshalling implementations for clojure & edn
(defmethod clj-> content-type-edn [_ body] (pr-str body))
(defmethod clj-> content-type-clj [_ body] (pr-str body))

(defmethod ->clj content-type-clj [_ body] (edn/read-string body))
(defmethod ->clj content-type-edn [_ body] (edn/read-string body))

;;; Other built-in marshallers
(defmethod ->clj content-type-form [_ body] (form-decode body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; functions that deal with ring request
(defn- realize-body
  "...consider implementing as delay...
  ...or as function on stream...
  ...or as future, promise or whatever works best..."
  ([body encoding]
   (if (string? body)
     body
     (slurp body :encoding encoding)))

  ([req]
   (realize-body req nil)))

(defn- set-content-type [resp mime]
  (let [mime (name mime)]
    (-> resp
        (assoc :content-type mime)
        (assoc-in headers-content-type mime))))

(defn- content-type
  [request]
  (when-some [parsed-content-type (some-> (or (:content-type request)
                                              (get-in request headers-content-type)
                                              content-type-default)
                                          (parse-mime))]
    (when-some [des (deserializable? (:mime parsed-content-type))]
      [(:mime parsed-content-type)
       (:charset parsed-content-type)
       (partial des nil)])))

(def ^:dynamic *use-default-mime-for* #{"*" "*/*" "text/*" "application/*"})

(defn- accept
  [request]
  (let [mime-types (some-> (or (get-in request headers-accept)
                               (get-in request [:headers "accept"])
                               *default-mime-type*)
                           (#(when (*use-default-mime-for* %) *default-mime-type*))
                           parse-accept)]

    (first (for [mime mime-types
                 :let [ser (serializable? (:mime mime))]
                 :when ser]
             [(:mime mime) (:charset mime) (partial ser nil)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the actual middleware
(defn wrap-content-negotiation
  ([handler {:keys [consumes produces]}]

   (let [consumes (set consumes)
         produces (set produces)]

     (fn [{:as request :keys [body]}]

       (let [[mime-in  charset-in  des] (when body (content-type request))
             [mime-out charset-out ser] (accept request)

             ;; Allow handlers to override serialization & deserialization
             des (if (consumes mime-in)  identity des)
             ser (if (produces mime-out) identity ser)]

         (cond
          (and body (nil? des)) (unsupported-media request)

          (nil? ser) (not-acceptable request)

          :else (let [body-in (some-> body
                                      (realize-body charset-in)
                                      (des))

                      resp (handler (assoc request :body body-in))

                      body-out (some-> (:body resp)
                                       (ser)
                                       (realize-body charset-out))]

                  (if body-out
                    (-> (assoc resp :body body-out)
                        (set-content-type mime-out))
                    resp)))))))

  ([handler] (wrap-content-negotiation handler {})))
