(ns somni.misc
  "Miscellaneous functions that don't have an obvious namespace."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [ring.util.codec :refer [url-decode]]))

(defn by-tag
  "For use when dispatching on a tagged tuple"
  [tag & _] tag)

(defmacro meta' [f] `(meta #'~f))

(defn thunk? [x]
  (or (instance? clojure.lang.IFn x)
      (instance? clojure.lang.IDeref x)))

(defprotocol Unthunk (unthunk [x]))
(extend-type clojure.lang.IDeref Unthunk (unthunk [x] @x))
(extend-type clojure.lang.Fn     Unthunk (unthunk [x] (x)))
(extend-type Object              Unthunk (unthunk [x]  x))

(defn non-empty? [x] (and (coll? x) (seq x) x))

(defn map-first [f xs] (map (fn [[a & b]] (cons (f a) b)) xs))

(defn uri->path [uri] (remove empty? (str/split (or uri "") #"/")))

(defn desc [a b] (compare b a))

(defn flip [f] (fn [& args] (apply f (reverse args))))

(defn map-init
  ([f ks] (reduce #(assoc %1 %2 (f %2)) {} ks))
  ([ks] (map-init (constantly nil) ks)))

(defn ->map [col] (reduce (fn [a [k v]] (assoc a k v)) {} col))

(defn get-header
  ([request header default]
   (or (get-in request [:headers header])
       (get-in request [:headers (str/lower-case header)])
       default))
  ([request header]
   (get-header request header nil)))

(def ^:private clj-data #{ \{ \[ \( })

(defn- decode-clj [[x :as xs]]
  (when (clj-data x) (edn/read-string xs)))

(def ^:private re-num #"([+-]?\d+\.?\d*([eE][+-]\d*)?)")

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
       (map (fn [[k v]] [(keyword k) (decode v)]))
       (vec)
       (into {})))
