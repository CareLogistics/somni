(ns somni.misc
  "Miscellaneous functions that don't have an obvious namespace."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn by-tag
  "For use when dispatching on a tagged tuple"
  [tag & _] tag)

(defn has-method [multi tag] ((methods multi) tag))

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

(defn str->map [s re]
  (->> (str/split s re)
       (remove empty?)
       (partition 2)
       (map (fn [[k v]] [(keyword k) (edn/read-string v)]))
       (vec)
       (into {})))

(defn desc [a b] (compare b a))

(defn flip [f] (fn [& args] (apply f (reverse args))))

(defn map-init
  ([f ks] (reduce #(assoc %1 %2 (f %2)) {} ks))
  ([ks] (map-init (constantly nil) ks)))

(defn ->map [col] (reduce (fn [a [k v]] (assoc a k v)) {} col))
