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

(def parse-mime
  (memoize
   (comp parse-charset
         parse-mime-params
         parse-media-type
         split-mime)))

(def ^:private accept-rexp #"\s*,\s*")

(defn- split-accept [a] (str/split (or a "") accept-rexp))

(defn parse-accept [a]
  (let [[h & t :as xs] (when (seq a) (map parse-mime (split-accept a)))]
    (cond
     t     (sort-mimes xs)
     h     [h]
     :else [{:group "*", :media :*}])))
