(ns somni.http.mime
  (:require [clojure.string :as str]
            [somni.misc :refer [desc]])
  (:import (java.nio.charset Charset)))

(defn- glob? [x] (= "*" x))

(defn- count-glob [m] (count (filter (comp glob? val) m)))

(defn- sort-mimes
  [xs]
  (->> xs
       (sort-by count-glob)
       (sort-by count desc)
       (sort-by :q    desc)))

(def ^:private charset-for-name
  (memoize
   (fn [charset]
     (if (and charset (Charset/isSupported charset))
       charset
       (str (Charset/defaultCharset))))))

(defn- parse-charset
  [{:as mime :keys [charset]}]
  (assoc mime :charset (charset-for-name charset)))

(defn- parse-mime-params
  [[mime & [params]]]
  (reduce (fn [a [k v]] (assoc a (keyword k) v))
          (assoc mime :q "1.0")
          (partition 2 params)))

(def ^:private mime-suffix-rexp #"\+")

(defn- parse-media-type [[group sub & params]]
  (let [[media suffix] (str/split (or sub "") mime-suffix-rexp)]
    [(merge {:group group,
             :media media
             :media-type (str group "/" media (when suffix (str "+" suffix)))}
            (when suffix {:suffix suffix}))
     params]))

(def ^:private mime-params-rexp #"\s*\/\s*|\s*\;\s*|\s*\=\s*")

(defn- split-mime [a] (str/split (or a "") mime-params-rexp))

(def ^:private accept-rexp #"\s*,\s*")

(defn- split-accept [a] (str/split (or a "") accept-rexp))

(def representation
  (memoize
   (comp parse-charset
         parse-mime-params
         parse-media-type
         split-mime)))

(defn accept
  [request]
  (let [accept-hdr     (get-in request [:headers "accept"] "*/*")
        [h & t :as xs] (map representation (split-accept accept-hdr))]
    (if t (sort-mimes xs) [h])))

(defn content-type
  [request]
  (representation (get-in request [:headers "content-type"]
                          "application/octet-stream")))
