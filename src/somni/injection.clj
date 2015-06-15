(ns somni.injection
  (:require [somni.misc :refer [thunk? unthunk meta']]))

(defmacro args->deps
  ""
  [f]
  `(seq (first (:arglists (meta' ~f)))))

(defmacro partial-by-name
  [f params]
  `(args->deps ~f))

(defn- wrap-thunked-deps
  [handler deps]
  (fn [request] (apply handler request (unthunk deps))))

(defn- wrap-deps
  [handler])

(defn test-fn-A [a b c])
(def test-data-A)
