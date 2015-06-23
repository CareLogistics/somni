(ns somni.router
  "
  A simple routing library based upon a trie.  Supports wildcards & keyword
  wildcards.
  "
  (:require [clojure.string :as str]
            [somni.misc :refer :all]
            [somni.http.errors :refer [not-found]]))

(defn bindings->wildcard
  [uri]
  (str/replace uri #":[^\/]+" "*"))

(defn- has-route?
  "determine if a router has an exact match for a route"
  [router op path]
  (op (get-in router path)))

(defn add-route
  "add a new route handler to a router"
  ([router op path handler]
   (let [path (map bindings->wildcard path)]
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
  (update-in router path dissoc op))

(defn find-handler
  "fast search for best possible match of a given path.
  returns handler for route or nil."
  ([router op [h & path-remaining] default]

   {:pre [(keyword? op)]}

   (let [exact-match (get router h)
         wild-match  (when-not exact-match (get router "*"))
         next-branch (or exact-match wild-match)
         handler     (or (op next-branch) (:any next-branch))
         default'    (if wild-match handler default)]

     (if (and path-remaining next-branch)
       (recur next-branch op path-remaining default')
       (or handler default'))))

  ([router op path] (find-handler router op path nil)))

(defn router->handler
  "converts a router to a ring handler."
  ([router on-missing]

   {:pre [(map? router)
          (ifn? on-missing)]}

   (fn [{:as request :keys [uri request-method]}]
     (let [path    (uri->path uri)
           router' (unthunk router)
           handler (find-handler router'
                                 request-method
                                 path
                                 on-missing)]
       (handler request))))

  ([router] (router->handler router not-found)))
