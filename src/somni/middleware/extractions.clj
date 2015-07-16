(ns somni.middleware.extractions
  (:require [clojure.string :as str]
            [somni.misc :refer [uri->path]]))

(defn extract
  ([] nil)
  ([obj [a & [b & z :as t] :as xpath]]
   (cond
     (empty? xpath) obj

     (nil? obj) obj

     (map? obj) (for [[k v] obj
                      :when (= (name k) a)
                      x (extract v t)]
                  x)

     (coll? obj) (flatten (for [i obj
                                :let [m (if (map? i) i (bean i))]
                                [k v] m
                                :when (= (name k) a)
                                :let [x (if b
                                          (when (= (str v) b)
                                            (extract m z))
                                          (extract v t))]
                                :when x]
                            (if (coll? x) x [x])))

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
