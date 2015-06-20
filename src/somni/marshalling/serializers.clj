(ns somni.marshalling.serializers
  (:require [somni.http.mime :refer [parse-accept]]))

(defn- by-mime-type [mime-type & _] mime-type)

(defmulti clj->
  "serialize based upon content-type chosen from accept header"
  by-mime-type)

(defmethod clj-> :*       [_ body] (pr-str body))
(defmethod clj-> :edn     [_ body] (pr-str body))
(defmethod clj-> :clojure [_ body] (pr-str body))

(defn request->accept
  [request]
  (some-> (or (get-in request [:headers "Accept"])
              (get-in request [:headers "accept"]))
          parse-accept))

(defn acceptable?
  [req]
  (seq (for [a (request->accept req)
             :let  [mime-type (:media a)]
             :when ((methods clj->) mime-type)]
         mime-type)))

(defn serializable? [req] (not-empty (:body req)))

(defn serialize
  [req])
