(ns somni.middleware.bindings)

(def ^:private bindings-rexp #"\w+|:(\w*)")

(defn wrap-bindings
  "
  Destructures the uri and binds segments into the request params.

  Example:
  ((wrap-bindings identity [[2 \"a\"][1 \"b\"]])
   {:uri \"/foo/bar/baz/quux\"})

  #_=> {:params {:b \"bar\", :a \"baz\"}, :uri \"/foo/bar/baz/quux\"}
  "
  [handler bindings]

  (fn [{:as request :keys [uri]}]
    (let [path (vec (uri->path uri))
          request (reduce
                   (fn [a [i k]] (assoc-in a [:params (keyword k)] (nth path i)))
                   request
                   bindings)]
      (handler request))))

(defn- get-binding-info
  "
  Extracts binding information from a uri.
  "
  [uri]
  (->> (re-seq bindings-rexp uri)
       (map second)
       (zipmap (range))
       (filter second)))

(defn- attach-bindings
  "
  Updates the resource with wrap-bindings middleware if bindings are defined
  for this resource.
  "
  [table]
  (for [[uri {:as details :keys [resource]}] table]

    [uri (if-some [bindings (seq (get-binding-info uri))]
           (assoc details
             :resource (with-meta
                         (wrap-bindings resource bindings)
                         (meta resource)))
           details)]))
