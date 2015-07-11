(ns somni.router
  "
  A simple routing library based upon a trie.  Supports wildcards & keyword
  wildcards.
  "
  (:require [clojure.string :as str]
            [somni.misc :refer :all]
            [somni.http.errors :refer [not-found
                                       unsupported-method]]))

(defn wildcards->globs
  [uri]
  (str/replace uri #"[:$][^\/]+(\/\?$)?|\?$" "*"))

(defn- has-route?
  "determine if a router has an exact match for a route"
  [router op path]
  (op (get-in router path)))

(defn- ->path [p]
  (cond
    (string? p) (uri->path (wildcards->globs p))
    (coll?   p) (map wildcards->globs p)))

(defn add-route
  "add a new route handler to a router"
  ([router op path handler]
   (let [path (->path path)]
     (if-some [existing (has-route? router op path)]
       (throw (ex-info "Routing conflict" {:existing existing}))
       (assoc-in router (concat path [op]) handler))))

  ([router [op path handler]] (add-route router op path handler)))

(defn add-routes
  "add routes to a router where routes is [path handler]"
  [router routes]
  (reduce add-route router routes))

(defn remove-route
  "remove a route handler from a router"
  [router op path]
  (update-in router (->path path) dissoc op))

(defn find-handler
  "fast search for best possible match of a given path.
  returns handler for route or nil."
  [router op path]

  {:pre [(keyword? op)]}

  (loop [router     router
         [h & path] path
         fall-back  nil]

    (let [matched   (get router h)

          fall-back (when-not matched (or (get router "*")
                                          fall-back))
          router    (or matched
                        fall-back)
          handler   (or (op   router)
                        (:any router))]

      (cond
       (and path router) (recur router path fall-back)
       (nil? path)       (or handler   :no-such-op)
       (nil? router)     (or fall-back :no-such-path)))))

(defn router->handler
  "converts a router to a ring handler."
  ([router on-missing]

   {:pre [(map? router)
          (ifn? on-missing)]}

   (fn [{:as request :keys [uri request-method]}]
     (let [path    (uri->path uri)
           router  (unthunk router)
           handler (find-handler router request-method path)]

       (condp = handler
         :no-such-op   (unsupported-method request)
         :no-such-path (on-missing request)
         (handler (assoc request :path path))))))

  ([router] (router->handler router not-found)))
