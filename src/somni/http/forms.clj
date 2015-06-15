(ns somni.http.forms
  (:require [clojure.string :as str]
            [somni.misc :refer [lazy->string]]
            [somni.middleware.negotiation :refer [->clj clj->]]))

(defn- str->map [s re]
  (->> (str/split s re)
       (remove empty?)
       (partition 2)
       (reduce (fn [a [k v]] (assoc a (keyword k) v))
               {})))

(def ^:private form-rexp #"\s*&\s*|\s*=\s*")

(defn form-decode
  "
  This is basic, no frills x-www-form-urlencoded deserializer to Clojure map,
  Keys as keywords, values as strings.
  "
  ([body encoding]
   (str->map (lazy->string body encoding) form-rexp))

  ([body]
   (form-decode body nil)))
