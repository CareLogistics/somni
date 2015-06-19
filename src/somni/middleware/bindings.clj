(ns somni.middleware.bindings
  (:require [somni.misc :refer [uri->path]]))

(def ^:private bindings-rexp #"\w+|:([^\/]*)")

(defn get-binding-info
  "Extracts binding information from a uri."
  [uri]
  (->> (re-seq bindings-rexp uri)
       (map second)
       (zipmap (range))
       (filter second)))

(defn- wrap-bindings*
  [handler bindings]
  (fn [{:as request :keys [uri]}]
    (let [path (vec (uri->path uri))
          req (reduce
               (fn [r [i n]] (assoc-in r [:params (keyword n)] (nth path i nil)))
               request
               bindings)]
      (handler req))))

(defn wrap-bindings
  "
  Takes a handler and a URI route to handler.  If that URI contains
  keywords, a request will bind those segments of a request URI to
  [:params :binding-keyword].

  Example:
  ((wrap-bindings identity \"/user/:user/:page/:sub-page\")
   {:uri \"/user/bob/profile/contacts\"})

  #_=> {:params {:user \"bob\", :page \"profile\", :sub-page \"contacts\"}}
  "
  [handler uri]

  {:pre [(ifn? handler)]}

  (if-some [bindings (get-binding-info uri)]
    (wrap-bindings* handler bindings)
    handler))
