(ns somni.http.mime
  (:require [clojure.string :as str])
  (:import (java.nio.charset Charset)))

(def ^:private accept-rexp #"\s*,\s*")

(defn- split-accept [a] (str/split (or a "") accept-rexp))

(def ^:private mime-params-rexp #"\s*\/\s*|\s*\;\s*|\s*\=\s*")

(defn- split-mime   [a] (str/split (or a "") mime-params-rexp))

(def ^:private mime-suffix-rexp #"\+")

(defn- parse-mime-type [[t s & params]]
  (let [[a b] (str/split (or s "") mime-suffix-rexp)]
    [(merge {:group t, :media (keyword a)}
            (when b {:suffix (keyword b)}))
     params]))

(defn- parse-mime-params
  [[mime & [params]]]
  (reduce (fn [a [k v]] (assoc a (keyword k) v))
          (assoc mime :q "1.0")
          (partition 2 params)))

(def ^:private default-charset (str (Charset/defaultCharset)))

(defn- parse-charset
  [{:as mime
    :keys [charset]
    :or {charset default-charset}}]
  (when (Charset/isSupported charset)
    (assoc mime :charset charset)))

(def parse-mime
  (memoize                              ; adds ~9x speed up
   (comp parse-charset
         parse-mime-params
         parse-mime-type
         split-mime)))

(defn- glob? [x] (= "*" (name x)))

(defn- count-glob [m] (count (filter (comp glob? val) m)))

(def ^:private desc #(compare %2 %1))

(defn- sort-mimes
  [xs]
  (->> xs
       (sort-by count-glob)
       (sort-by count desc)
       (sort-by :q    desc)))

(defn parse-accept [a]
  ;;; NOTE: accept have no fixed ordering, so pointless to memoize
  (let [[h & t :as xs] (when (seq a) (map parse-mime (split-accept a)))]
    (cond
     t     (sort-mimes xs)
     h     [h]
     :else [{:group "*", :media :*}])))
