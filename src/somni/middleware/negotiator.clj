(ns somni.middleware.negotiator
  (:require [somni.http.errors :refer :all]
            [somni.http.forms :refer [form-decode]]
            [clojure.edn :as edn]
            [somni.http.mime :refer [parse-mime parse-accept]]
            [somni.misc :refer [by-tag get-header]]
            [ring.util.request :as req]
            [ring.util.response :as resp]
            [clojure.pprint :refer [pprint]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; marshalling interface
(defmulti ->clj
  "deserialize based upon content-type of request"
  by-tag)

(defmulti clj->
  "serialize based upon content-type chosen from accept header"
  by-tag)

(def deserializable? (partial get-method ->clj))
(def serializable?   (partial get-method clj->))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string constants
(def ^:const content-type-default "application/octet-stream")
(def ^:const content-type-any     "*/*")
(def ^:const content-type-edn     "application/edn")
(def ^:const content-type-clj     "application/clojure")
(def ^:const content-type-form    "application/x-www-form-urlencoded")

(def ^:dynamic *default-mime-type* content-type-edn)
(defn set-default-mime-type!
  [mime-type]
  (when ((methods clj->) mime-type)
    (alter-var-root #'*default-mime-type* (fn [_] mime-type))))

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
  [body encoding]
  (if (string? body)
    body
    (slurp body :encoding encoding)))

(defn- select-deserializer
  [request]
  (when-some [parsed-content-type (parse-mime
                                   (get-header request
                                               "Content-Type"
                                               content-type-default))]
    (when-some [des (deserializable? (:mime parsed-content-type))]
      [(:mime parsed-content-type)
       (:charset parsed-content-type)
       (partial des nil)])))

(def ^:dynamic *wildcard-mime-types* #{"*" "*/*" "text/*" "application/*"})

(defn- wildcards->default-mime-type
  [{:as mime-type :keys [mime]}]
  (if (*wildcard-mime-types* mime)
    (assoc mime-type :mime *default-mime-type*)
    mime-type))

(defn- select-serializer
  [request]
  (let [mime-types (some->> (get-header request "Accept" *default-mime-type*)
                            (parse-accept)
                            (map wildcards->default-mime-type))]

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

     (fn [request]

       (let [content-length (req/content-length request)
             [mime-in  charset-in  des] (select-deserializer request)
             [mime-out charset-out ser] (select-serializer   request)

             ;; Allow handlers to override serialization & deserialization
             des (if (consumes mime-in)  identity des)
             ser (if (produces mime-out) identity ser)]

         (cond
          (and content-length
               (> content-length 0)
               (nil? des)) (unsupported-media request)

          (nil? ser) (not-acceptable request)

          :else (let [body-in (when content-length
                                (-> (:body request)
                                    (realize-body charset-in)
                                    (des)))

                      resp (handler (assoc request :body body-in))]

                  (if-some [body-out (:body resp)]
                    (-> (assoc resp :body (ser body-out))
                        (resp/content-type mime-out))
                    resp)))))))

  ([handler] (wrap-content-negotiation handler {})))
