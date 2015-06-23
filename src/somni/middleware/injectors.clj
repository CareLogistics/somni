(ns somni.middleware.injectors
  (:require [somni.misc :refer [unthunk]]))

(defn- names->symbols
  [m]
  (reduce (fn [a [k v]] (assoc a (symbol (name k)) v)) {} m))

(defn- partial-from-map
  [f-var m-args]
  (let [m-args   (names->symbols m-args)
        f-meta   (meta f-var)
        arglists (:arglists f-meta)
        matched  (first (filter #(every? m-args %) arglists))
        params   (seq (map m-args matched))]

    (if params
      (fn []   (eval `(~f-var ~@params)))
      (fn [m2] (partial-from-map f-var (merge m-args m2))))))

(defn- extract-as
  ([k as] (fn [m] (when-some [v (m k)] {as v})))
  ([k] (extract-as k k)))

(def ^:dynamic *dep-generators*
  "
  Functions that extract portions of a ring request to be injected as deps
  that will be injected by name into a wrap-deps function.

  Default extraction/injections are:
  * request      - request argument
  * body         - arguments named body, data & payload
  * headers      - header argument
  * query-params - by query-param's name
  * params       - by parameter's name
  * identity     - by identity parameter's name; e.g., 'user' & 'role'
  "
  [(fn [r] {:request r, :req r, :r r})
   (extract-as :body)
   (extract-as :body :data)
   (extract-as :body :payload)
   (extract-as :headers)
   (extract-as :params)
   (extract-as :query-params)
   :query-params
   :params
   :identity])

(defn- request->deps
  [request]
  (reduce (fn [a f] (merge a (f request)))
          {}
          *dep-generators*))

(defn inject-deps-into-request
  "..."
  [handler deps]
  (fn [request]
    ((partial-from-map handler
                       (merge (unthunk deps)
                              (request->deps request))))))
