(ns somni.misc
  "
  Miscellaneous functions that don't have an obvious namespace.
  "
  (:require [clojure.string :as str]))

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

(defn lazy->string
  ([body encoding]
   (if (string? body) body (slurp body :encoding encoding)))

  ([body]
   (lazy->string body nil)))
