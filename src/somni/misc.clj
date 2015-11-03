;;; Copyright (c) Care Logistics, inc. All rights reserved.
;;; The use and distribution terms for this software are covered by the
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.html at the root of this
;;; distribution.
;;; By using this software in any fashion, you are agreeing to be bound by
;;; the terms of this license.
;;; You must not remove this notice, or any other, from this software.

(ns somni.misc
  "Miscellaneous functions that don't have an obvious namespace."
  (:require [camel-snake-kebab.core :refer [->kebab-case]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [ring.util.codec :refer [url-decode]]))

(defn thunk? [x]
  (or (instance? clojure.lang.IFn x)
      (instance? clojure.lang.IDeref x)))

(defprotocol Unthunk (unthunk [x]))
(extend-type clojure.lang.IDeref Unthunk (unthunk [x] @x))
(extend-type clojure.lang.Fn     Unthunk (unthunk [x] (x)))
(extend-type Object              Unthunk (unthunk [x]  x))

(defn uri->path [uri] (remove empty? (str/split (or uri "") #"/")))

(defn greedy-path?
  [path]
  (when (#{\* \?} (last path))
    (str/join (butlast path))))

(defn desc [a b] (compare b a))

(def ^:private clj-data #{ \{ \[ \( })

(defn- decode-clj [[x :as xs]]
  (when (clj-data x) (edn/read-string xs)))

(def ^:private re-num #"^[+-]?([1-9]{1}\d*|0{1})(\.\d*)?{0,1}([Ee][+-]?\d+)?$")

(defn- decode-num [xs]
  (when-some [[[num-str]] (re-seq re-num xs)] (edn/read-string xs)))

(defn decode [xs]
  (when (seq xs)
    (let [xs (url-decode xs)]
      (or (decode-clj xs)
          (decode-num xs)
          xs))))

(defn str->map [s re]
  (->> (str/split s re)
       (remove empty?)
       (partition 2)
       (map (fn [[k v]] [(keyword (->kebab-case k)) (decode v)]))
       (vec)
       (into {})))

(defn safe-name [x]
  (cond
    (string? x)                      x
    (instance? clojure.lang.Named x) (name x)
    :else                            (str x)))
