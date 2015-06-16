(ns somni.injectors
  (:require [somni.misc :refer [unthunk]]))

(defn partial-from-map
  [f m]
  (let [m (reduce (fn [a [k v]] (assoc a (symbol (name k)) v)) {} m)
        {:as f-meta :keys [arglists]} (meta f)
        matched (first (filter #(every? m %) arglists))
        params (seq (map m matched))]

    (with-meta
      (if params
        (fn []   (eval `(~f ~@params)))
        (fn [m2] (partial-from-map f (merge m m2))))
      f-meta)))

(def ^:dynamic *dep-generators* {:body     :body
                                 :headers  :headers
                                 :identity :identity
                                 :params   :params
                                 :request  identity})

(defn- request->deps
  [request]
  (reduce (fn [a [k f]] (if-some [v (f request)]
                         (assoc a k v)
                         a))
          nil
          *dep-generators*))

(defn wrap-deps*
  ([handler deps dep-gen]
   (with-redefs [*dep-generators* dep-gen]
     (fn [request]
       ((partial-from-map handler
                          (merge (unthunk deps)
                                 (request->deps request)))))))

  ([handler deps] (wrap-deps* handler deps *dep-generators*)))

(defmacro wrap-deps
  ([handler deps dep-gen] `(wrap-deps* #'~handler ~deps ~dep-gen))
  ([handler deps]         `(wrap-deps* #'~handler ~deps)))
