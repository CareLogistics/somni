(ns somni.middleware.extractions
  (:require [camel-snake-kebab.core :refer [->kebab-case]]
            [clojure.string :as str]
            [somni.misc :refer [uri->path]]))

(defn- extract*
  [obj xpath]
  (cond
    (empty? xpath) [obj]

    (nil? obj) []

    (map? obj) (for [[k v] obj
                     :when (= (name k) (first xpath))
                     x (extract* v (next xpath))]
                 x)

    (coll? obj) (for [i obj
                      :let [m (if (map? i) i (bean i))]
                      [k v] m
                      :when (= (name k) (first xpath))
                      :let [b (second xpath)]
                      x (if (and b (= (str v) b))
                          (extract* m (drop 2 xpath))
                          (extract* v (next xpath)))]
                  x)

    :else (extract* (bean obj) xpath)))

(defn extract
  [obj xpath]
  (when-some [r (extract* obj xpath)]
    (if (coll? r)
      (when (seq r) r)
      r)))

(defn- wrap-extractions*
  [handler skip-fn]
  (fn [{:as request :keys [uri path request-method]}]
    (let [response (handler request)]
      (if (= :get request-method)
        (extract response (map ->kebab-case (skip-fn (or path (uri->path uri)))))
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
