(ns somni.middleware.injectors
  (:require [somni.misc :refer [unthunk desc map-first ->map]]))

(defn- names->symbols [m] (->map (map-first (comp symbol name) m)))

(defn- arglists [var] (:arglists (meta var)))

(defn- best-match
  "Best matched is shortest arglist that is most fully satisfied."
  [arglists m-args]
  (->> (map #(filter m-args %) arglists)
       (zipmap arglists)
       (vec)
       (sort-by (comp count first))
       (sort-by (comp count second) desc)
       (ffirst)
       (map m-args)))

(defn- partial-from-map
  "Partially applies a function with arguments matched by name from
  a keys in m-args.  Returns a function that can be invoked with arguments
  already applied, or with 0-arity to invoke best matched arity of original
  function."
  [f-var m-args]
  (let [m-args (names->symbols m-args)]
    (fn
      ([m2] (partial-from-map f-var (merge m-args m2)))
      ([] (apply f-var (best-match (arglists f-var) m-args))))))

(defn- extract-as
  ([k as] (fn [m] (when-some [v (m k)] {as v})))
  ([k] (extract-as k k)))

(def ^:dynamic *dep-generators*
  "
  Functions that extract portions of a ring request to be injected as deps
  that will be injected by name into a wrap-deps function.

  Default extraction/injections are:
  * request      -> request
  * body         -> body, 'data and 'payload
  * headers      -> header
  * query-params -> query-params & keys to args
  * params       -> params & keys to args
  * identity     -> identity & keys to args
  "
  [(fn [r] {:request r, :req r, :r r})
   (extract-as :body)
   (extract-as :body :data)
   (extract-as :body :payload)
   (extract-as :body :entity)
   (extract-as :headers)
   (extract-as :params)
   (extract-as :query-params)
   (extract-as :identity)
   :query-params
   :params
   :identity])

(defn- generate-deps [r] (reduce #(merge %1 (%2 r)) {} *dep-generators*))

(defn inject-deps-into-handler
  "
  Injects depenendencies into a handler function, based upon deps provided
  as well as dependencies generated from the request.

    handler - must var or otherwise have meta that includes arglists
    deps    - is a map of named items to values

  See *dep-generators* - a vector of functions that take a map & return a map.
  The result of each dep-generator function will be merged into request before
  handler is invoked.

  This allows handler functions to be written in an application's business
  domain rather than as ring specific handlers.

  Example: (defn hello [{:as request :keys [body params] ...}] (let [...]...))
  becomes: (defn hello [user body param1 param2] ...)
  where:
  * param1 & param2 are lifted from params
  * user is lifted from request identity
  * body is lifted from request

  Additionally, dependencies required by a handler - such as database, other
  services, etc - can be applied through this middleware.  This is an alternative
  to boxing your functions in componenent or utilizing global vars to pass these
  values.
  "
  [handler deps]
  (fn [request]
    ((partial-from-map handler
                       (merge (unthunk deps)
                              (generate-deps request))))))
