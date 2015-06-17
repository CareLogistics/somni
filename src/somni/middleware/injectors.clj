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

(defn- view-only [k] (fn [m] (when-some [v (m k)] {k v})))

(def ^:dynamic *dep-generators* {:body     (view-only :body)
                                 :headers  (view-only :headers)
                                 :identity :identity
                                 :params   :params})

(defn- request->deps
  [request]
  (reduce (fn [a [k f]] (merge a (f request)))
          {:request request}
          *dep-generators*))

(defn wrap-deps*
  "..."
  ([handler deps dep-gen]
   (with-redefs [*dep-generators* dep-gen]
     (fn [request]
       ((partial-from-map handler
                          (merge (unthunk deps)
                                 (request->deps request)))))))

  ([handler deps] (wrap-deps* handler deps *dep-generators*)))

(defmacro wrap-deps
  "..."
  ([handler deps dep-gen] `(wrap-deps* #'~handler ~deps ~dep-gen))
  ([handler deps]         `(wrap-deps* #'~handler ~deps)))
