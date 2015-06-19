(ns somni.http.forms
  (:require [somni.misc :refer [str->map]]))

(def ^:private form-rexp #"\s*&\s*|\s*=\s*")

(defn form-decode
  "This is basic, no frills x-www-form-urlencoded deserializer to Clojure map,
  Keys as keywords, values as clojure data.
  "
  [body] (str->map body form-rexp))
