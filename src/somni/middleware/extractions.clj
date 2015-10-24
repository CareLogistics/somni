;;; Copyright (c) Care Logistics, inc. All rights reserved.
;;; The use and distribution terms for this software are covered by the
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.html at the root of this
;;; distribution.
;;; By using this software in any fashion, you are agreeing to be bound by
;;; the terms of this license.
;;; You must not remove this notice, or any other, from this software.

(ns somni.middleware.extractions
  (:require [camel-snake-kebab.core :refer [->kebab-case]]
            [somni.misc :refer [uri->path safe-name]]))

(defn- key-match [k [h]] (= (->kebab-case h) (->kebab-case (safe-name k))))

(defn- val-match [v [_ b]] (= b (safe-name v)))

(defn- as-map [x] (if (map? x) x (bean x)))

(defn- extract*
  [obj xpath]
  (cond
    (empty? xpath) [obj]

    (nil? obj)     []

    (map? obj)     (for [[k v] obj
                         :when (key-match k xpath)
                         x (extract* v (next xpath))]
                     x)

    (coll? obj)    (distinct
                    (for [i obj
                          :let [m (as-map i)]
                          [k v] m
                          :when (key-match k xpath)
                          x (if (val-match v xpath)
                              (extract* [m] (drop 2 xpath))
                              (extract*  v  (drop 1 xpath)))]
                      x))

    :else          (extract* (bean obj) xpath)))

(defn extract
  [obj xpath]
  (when-some [r (extract* obj xpath)]
    (if (coll? r)
      (when (seq r) (flatten r))
      r)))

(defn- wrap-extractions*
  [handler skip-fn]
  (fn [{:as request :keys [uri path request-method]}]
    (let [response (handler request)]
      (if (= :get request-method)
        (extract response
                 (skip-fn (or path
                              (uri->path uri))))
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
