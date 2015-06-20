(ns somni.marshalling.deserializers
  (:require [somni.http.mime :refer [parse-mime]]
            [somni.http.forms :refer [form-decode]]
            [somni.misc :refer [realize-string]]
            [clojure.edn :as edn]))

(defn- by-mime-type [mime-type & _] mime-type)

(defmulti ->clj
  "deserialize based upon content-type of request"
  by-mime-type)

(defmethod ->clj :edn     [_ body] (edn/read-string body))
(defmethod ->clj :clojure [_ body] (edn/read-string body))
(defmethod ->clj :x-www-form-urlencoded [_ body] (form-decode body))

(def ^:const default-content-type (doto "application/octet-stream"
                                    (.intern)))

(defn request->content-type
  [req]
  (some-> (or (:content-type req)
              (get-in req [:headers "Content-Type"]))
          parse-mime
          :media))

(defn deserializable?
  [{:as req :keys [body]}]
  (when body
    (when-some [mime-type (request->content-type req)]
      (when ((methods ->clj) mime-type)
        mime-type))))

(defn deserialize
  [req]
  (if-some [mime-type (deserializable? req)]
    (update-in req [:body] (comp (partial ->clj mime-type)
                                 realize-string))
    req))
