(ns somni.injectors
  (:require [somni.misc :refer [unthunk]]))

(defn var->args
  [f]
  (:arglists (meta f)))

(defn invoke-with-map
  [f m]
  (let [m (reduce (fn [a [k v]] (assoc a (symbol (name k)) v)) {} m)
        {:as m :keys [arglists]} (meta f)
        matched (first (filter #(every? m %) arglists))
        params (seq (map m matched))]
    (if params
      (eval `(~f ~@params))
      (with-meta (fn [m2] (invoke-with-map f (merge m m2))) m))))

(defn wrap-deps
  [handler deps]
  (fn [{:as request :keys [body headers]}]
    ((assoc (unthunk deps)
       :request request
       :body    body
       :headers headers))))
