(ns somni.middleware.extractions
  (:require [clojure.string :as str]
            [somni.misc :refer [uri->path]]))

(defn- extract-map
  [m [h & t]]
  (first (for [[k v] m :when (= (name k) h)] [v t])))

(defn- extract-col
  [col [h i & t]]
  (first (for [m col
               :let [m (if (map? m) m (bean m))]
               [k v] m
               :when (= (name k) h)
               :when (= (str v) i)]
           [m t])))

(defn- extract
  ([] nil)
  ([obj xpath]
   (cond
     (empty? xpath) obj
     (map? obj) (apply extract (extract-map obj xpath))
     (coll? obj) (apply extract (extract-col obj xpath))
     :else (extract (bean obj) xpath))))

(defn- wrap-extractions*
  [handler skip-fn]
  (fn [{:as request :keys [uri path request-method]}]
    (let [response (handler request)]
      (if (= :get request-method)
        (extract response (skip-fn (or path (uri->path uri))))
        response))))

(defn- mk-skip-fn [uri]
  (let [path (uri->path uri)]
    (when (= "?" (last path))
      (partial drop (dec (count path))))))

(defn wrap-extractions
  "This middleware extracts nested items from data based upon the
  unmatched portions of a uri."
  [handler uri]
  (if-some [skip-fn (mk-skip-fn uri)]
    (wrap-extractions* handler skip-fn)
    handler))
