(ns somni.middleware.injectors
  (:require [somni.misc :refer [unthunk desc map-first ->map]]))

(defn- names->symbols [m] (->map (map-first (comp symbol name) m)))

(defn- arglists [var]
  (for [arglist (:arglists (meta var))]
    (for [x arglist]
      (cond
        (symbol? x) x
        (map? x) (:as x)
        (coll? x) (second (drop-while #(not= :as %) x))))))

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

(defn- merge-non-nil
  [& maps]
  (let [acc (transient (hash-map))]
    (doseq [m maps :when (map? m),
            e m :when (val e)]
      (conj! acc e))
    (persistent! acc)))

(defn- request->deps
  [{:as request
    :keys [identity bindings body params query-params headers]}]
  (cond-> (merge-non-nil query-params params body bindings identity)
    :always (assoc :request request :req request :r request)
    headers (assoc :headers headers)
    body    (assoc :data body :payload body :entity body)))

(defn context-aware
  [x]
  {:pre [(ifn? x)]}
  [::ctx x])

(defn- context-aware?
  [x]
  (when (and (coll? x)
             (= ::ctx (first x)))
    (second x)))

(defn- add-context
  [request dep]
  (if-some [ctx-dep (context-aware? dep)]
    (ctx-dep request)
    dep))

(defn- with-context
  [request deps]
  (reduce (fn [a [k v]] (assoc a k (add-context request v)))
          {}
          deps))

(defn inject-deps
  "Applies handler args by name from request & dependencies.  Dependencies
  that are wrapped in request-aware will be passed the request through function
  invocation prior to the dependency being passed to the handler.

  This enables using non-ring aware functions as ring handlers.  For example,
    (defn say [uri db] (db uri))
    (def dsay (inject-deps #'say {:db prn}))
    (dsay {:uri \"hello\" })
  "
  ([handler deps]
   (fn [request]
     (let [request (request->deps request)
           deps (with-context request deps)]
       ((partial-from-map handler (merge request deps)))))))
