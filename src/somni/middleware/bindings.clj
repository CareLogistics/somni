(ns somni.middleware.bindings
  (:require [somni.misc :refer [uri->path decode]]))

(def ^:private bindings-rexp #"\w+|([:$])([^\/]*)")

(def val-fn {":" decode, "$" identity})

(defn get-binding-info
  "Extracts binding information from a uri."
  [uri]
  (for [[i [_ x k]] (zipmap (range) (re-seq bindings-rexp uri))
        :let [f (val-fn x)]
        :when f]
    [i (keyword k) f]))

(defn- wrap-bindings*
  [handler bindings]
  (fn [{:as request :keys [uri]}]
    (let [path (vec (uri->path uri))
          req (reduce
               (fn [r [i k f]] (assoc-in r [:bindings k]
                                        (f (nth path i nil))))
               request
               bindings)]
      (handler req))))

(defn attach-bindings
  "Bindings are extracted from a uri and added to request { :bindings {} }

  Uses a simple language for specifying binding in a uri:

  * \"uri/with/:keyword/bindings\" uses a decode to Clojure to set
    {:bindings {:keyword 1111}}

  * \"uri/with/$string/forced/bindings\" extracts uri segment as is into
    {:bindings {:string \"value\"}}

  Example: URI path of \"profiles/:user-id/cell-phone/$phone\"
  would extract \"profiles/sam/cell-phone/5555555555\" to
  {:bindings {:user-id \"sam\", :phone \"5555555555\"}}

  and extract \"profiles/1234/cell-phone/wag-the-dogs\" to
  {:bindings {:user-id 1234, :phone \"wag-the-dogs\"}}"
  [handler uri]

  {:pre [(ifn? handler)]}

  (if-some [bindings (get-binding-info uri)]
    (wrap-bindings* handler bindings)
    handler))
