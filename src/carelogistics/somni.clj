;;; Copyright (c) Care Logistics, inc. All rights reserved.
;;; The use and distribution terms for this software are covered by the
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.html at the root of this
;;; distribution.
;;; By using this software in any fashion, you are agreeing to be bound by
;;; the terms of this license.
;;; You must not remove this notice, or any other, from this software.

(ns ^{:author "Andrew Garman"}
  carelogistics.somni
  "
  An opinionated yet lightweight services routing library.

  Though it provides several Ring middleware functions, the correct way
  to use this middleware is through the make-handler function.

  Usage:

  (make-handler
   ;; resource definitions
   [{:uris [\"A/*/:label\",     ; REQUIRED: routing supports: alternate uris,
            \"B/:label\"        ; wildcards through * and keywords, &
            \"C/D/:lable\"],    ; params binding in case of keywords.

     :security :sec,          ; specifies an authentication function
                              ; required by the resource.  The fn type is
                              ; request -> identity; nil is unauthenticated.
                              ; when identity found it's assoc to :identity
                              ; in request

     :ops [:get, :put],       ; restricts http operations available

     :get [:valid :roles],    ; enforces ACLs, expects to find role
                              ; in request [:identity :role]
                              ; ACLs can be set for :get, :put, :post & :delete

     :handler :handler-name   ; will look for the handler function by this
                              ; name, first in handlers then via resolve

     :schema :example         ; schema that is checked upon
   }]
   ;; handlers map
   {:handler-name handler-fn} ; the lookup map for handler functions defined
                              ; in the resource definitions

   ;; options
   {:sec-handlers             ; the lookup map for security handler functions
     {:sec sec-fn},           ; defined in the resource definitions

    ;; there are other options available, see (doc make-handler)
   })

  Note: if the arguments passed to make-handler are invalid or inconsistent,
  an AssertionError will be thrown.  This is a better default than returning a
  broken handler that behaves in surprising and unexpected ways.

  (add-prefix prefix resources) ; adds a base path to all URIs in resources

  It's the stance of this library that content negotiation belongs in another
  library.  Until the author of this library needs to solve that problem, it's
  the recommendation that Accepts processing belongs in middleware that can be
  inserted into the on-request parameter of make-handler.

  Marshalling also belongs in another library.  Marshalling of inbound requests
  belongs in on-request and outbound response in on-response.
  "
  (:require [schema.core    :as s]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.edn    :as edn])
  (:import (java.nio.charset Charset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support functions

(def request-methods #{:get, :put, :post, :delete})

(defn- uri->path [uri] (remove empty? (str/split (or uri "") #"/")))

(defn- map-first [f xs] (map (fn [[a & b]] (cons (f a) b)) xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Client-side errors

(defn malformed-request
  [{:as r :keys [dev-mode]}]
  (when dev-mode (pp/write r))
  {:status 400, :body "Malformed request"})

(defn not-authenticated  [_] {:status 401, :body "Authentication required"})
(defn access-denied      [_] {:status 403, :body "Access denied"})
(defn not-found          [_] {:status 404, :body "Not found"})
(defn unsupported-method [_] {:status 405, :body "Unsupported HTTP method"})
(defn not-acceptable     [_] {:status 406, :body "Not Acceptable"})
(defn unsupported-media  [_] {:status 415, :body "Unsupported content-type"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server-side errors

(defn- server-error? [status] (and (number? status)
                                   (>= status 500)
                                   (<  status 600)
                                   status))

(defn server-error
  "
  This is a general purpose server error.  It includes r either in console
  *out* or in the body of the http response.  If there is a server error
  :status set in r, it will be the :status used in the response.
  "
  [{:as r :keys [status]} & [dev-mode]]

  {:status (or (server-error? status) 500)
   :body (if (or dev-mode (:dev-mode r))
           (pp/write r :stream nil)
           (do (pp/write r)
               "Internal server error"))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Middleware

(defn ex-details
  "
  Creates a pretty printed string for an exception's message, stack
  trace and cause.
  "
  [^Throwable e]

  (let [details {:exception  (type e)
                 :message    (.getMessage e)
                 :stackTrace (remove #(re-matches #"^(clojure|java).*" %)
                                     (map str (.getStackTrace e)))}]

    (if-some [cause (.getCause e)]
      (assoc details :cause (ex-details cause))
      details)))

(defn wrap-exception-handling
  "
  Returns a function that takes an r (request or response).  It invokes next-fn
  with r, catches exceptions thrown by next-fn.

  on-error is invoked with r merged with ex-data and ex-details added as :error.
  "
  [next-fn on-error]

  {:pre [next-fn on-error]}

  (fn [r]
    (let [x (try (next-fn r) (catch Exception e e))]
      (if (instance? Throwable x)
        (on-error (assoc (merge r (ex-data x))
                    :error (ex-details x)))
        x))))

(defn wrap-access-control
  "
  Wraps the handler in an ACL check.  ACLs are specified on http
  methods.  If acl-fn is nil, the user's role will be extracted
  from [:identity :role] of the request.

  Roles are compared by name; :abc, 'abc & \"abc\" are the same role.

  Returns 403 if ACL check fails.
  "
  [handler acls & [acl-fn policy]]

  {:pre [handler]}

  (let [role-lookup (or acl-fn #(get-in % [:identity :role]))
        acls (into {} (for [[k v] acls] [k (set (map name v))]))]

    (cond
     (seq acls) (fn [{:as request :keys [request-method]}]
                  (let [macl (get acls request-method)
                        role (role-lookup request)
                        role (and role (name role))]

                    (if (or (nil? macl)
                            (macl role))
                      (handler request)
                      (access-denied request))))

     (= policy :deny) access-denied
     :else            handler)))

(defn wrap-supported-methods
  "
  Returns 405 if an unsupported http method is made in the request.
  "
  [handler ops]

  {:pre [handler]}

  (let [ops (set ops)]
    (fn [{:as request :keys [request-method]}]
      (if-not (get ops request-method)
        (unsupported-method request)
        (handler request)))))

(defn wrap-schema-validation
  "
  Returns 400 if the body of a pust, put or patch does not match the
  schema.
  "
  [handler schema]

  {:pre [handler schema]}

  (fn [{:as request :keys [body request-method]}]
    (if-some [errors (and (#{:post, :put, :patch} request-method)
                          (s/check schema body))]
      (malformed-request (assoc request :errors errors))
      (handler request))))

(defn wrap-authentication
  "
  Extremely simple authentication.  If the security function returns
  a value, that value is associated with the request as the identity
  and the handler will be invoked.

  Returns a 401 if sec-fn returns nil.
  "
  [handler sec-fn]

  {:pre [handler sec-fn]}

  (fn [request]
    (if-some [id (sec-fn request)]
      (handler (assoc request :identity id))
      (not-authenticated request))))

(defn wrap-options
  "
  Returns 200 with body that describes the resource.
  "
  [handler resource & [schemas]]

  {:pre [handler resource]}

  ;; TODO: make something pretty out of that resource definition
  (fn [{:as request :keys [request-method]}]
    (if (= request-method :options)
      {:status       200
       :content-type "application/text"
       :body         resource}
      (handler request))))

(defn wrap-deps
  "
  Injects a handler's dependencies into its requests
  "
  [handler deps]
  (fn [request] (apply handler request deps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Binding middleware

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing mime-types and accept header

(def ^:private accept-rexp #"\s*,\s*")

(defn- split-accept [a] (str/split (or a "") accept-rexp))

(def ^:private mime-params-rexp #"\s*\/\s*|\s*\;\s*|\s*\=\s*")

(defn- split-mime   [a] (str/split (or a "") mime-params-rexp))

(def ^:private mime-suffix-rexp #"\+")

(defn- parse-mime-type [[t s & params]]
  (let [[a b] (str/split (or s "") mime-suffix-rexp)]
    [(merge {:group t, :media (keyword a)}
            (when b {:suffix (keyword b)}))
     params]))

(defn- parse-mime-params
  [[mime & [params]]]
  (reduce (fn [a [k v]] (assoc a (keyword k) v))
          mime
          (partition 2 params)))

(def ^:private default-charset (str (Charset/defaultCharset)))

(defn- parse-charset
  [{:as mime
    :keys [charset]
    :or {charset default-charset}}]
  (when (Charset/isSupported charset)
    (assoc mime :charset charset)))

(def parse-mime
  (memoize                              ; adds ~9x speed up
   (comp parse-charset
         parse-mime-params
         parse-mime-type
         split-mime)))

;;; NOTE: accpept have no fixed ordering, so pointless to memoize
(defn parse-accept [a]
  (let [[h & t :as xs] (when (seq a) (map parse-mime (split-accept a)))]
    (cond
     t     (sort-by #(:q % "1.0") #(compare %2 %1) xs)
     h     [h]
     :else [{:group "*", :media :*}])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Built-in media handlers, edn, x-www-form-urlencoded & "*"

(defn- body->str
  "
  Ridiculous helper added to make body params easier to test in the REPL.
  "
  [body & [encoding]]
  (if (string? body) body (slurp body :encoding encoding)))

(defn- str->map [s re]
  (->> (str/split s re)
       (remove empty?)
       (partition 2)
       (reduce (fn [a [k v]] (assoc a (keyword k) v))
               {})))

(def ^:private form-rexp #"\s*&\s*|\s*=\s*")

(defn form-decode
  "
  This is basic, no frills x-www-form-urlencoded deserializer to Clojure map,
  Keys as keywords, values as strings.
  "
  [body & [encoding]]
  (str->map (body->str body encoding) form-rexp))

(def builtin-media-handlers
  [{:supports  #{:octet-stream, :*}
    :mime-type "application/octet-stream"
    :from      identity}

   {:supports  #{:x-www-form-urlencoded}
    :mime-type "application/x-www-form-urlencoded"
    :to        form-decode}

   {:supports  #{:edn :clojure}
    :mime-type "application/edn"
    :from      pr-str
    :to        edn/read-string}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Content negotiation - only supports media type negotiation

(defn- media-handler<->mime
  "
  Associates a media handler to a mime type or returns nil.
  "
  [mhs mime]
  (letfn [(mh-lookup [mime-value]
            (first (filter #(get (:supports %) mime-value) mhs)))]

    (when-some [mh (or (mh-lookup (:media  mime))
                       (mh-lookup (:suffix mime)))]

      (merge mime mh))))

(defn- lookup-serializers
  "
  Returns all mime-types with associated media handlers that are acceptable.
  "
  [mhs accept]
  (for [mime (parse-accept accept)
        :let [mime (media-handler<->mime mhs mime)]
        :when (:from mime)]
    mime))

(defn- lookup-deserializer
  "
  Returns a deserializer for the given content type or nil.
  "
  [mhs content-type]
  (let [mh (media-handler<->mime mhs (parse-mime content-type))]
    (when (:to mh) mh)))

(defn- wrap-content-negotiation
  "
  Checks that content negotition can be fulfilled for the request.  If neither
  the accept or content-type are usable, middleware returns status 406 and 415
  respectively.
  "
  [handler media-handlers]

  ^{:mhs media-handlers}
  (fn [{:as request
       :keys [body content-type]
       :or   {content-type "application/octet-stream"}
       {:strs [accept Accept]} :headers}]

    (let [[ser] (lookup-serializers media-handlers (or accept Accept))
          des (lookup-deserializer media-handlers content-type)]

      (cond
       (and body (not des)) (unsupported-media request)

       (nil? ser) (not-acceptable request)

       :else (merge (handler request)
                    ;; Need to attach something here as the serializer
                    ;; won't have access to the original request.
                    (when ser {::mime ser}))))))

(defn- wrap-serialization
  "
  Serializes body of response with best available media handler.
  "
  [next-fn media-handlers]

  ^{:mhs media-handlers}
  (fn [{:as response
       :keys [body ::mime status headers]}]

    (cond
     (nil? body) (next-fn response)     ; Nothing to serialize

     (or (get headers "content-type")   ; TODO: + handlers control Accepts
         (get headers "Content-Type")) (next-fn response)

     (nil? mime) (if (>= status 400)    ; errors skip serialization
                   (next-fn response)
                   (server-error response))

     ;; TODO: do the correct thing with the charset
     :else (let [{:keys [from charset mime-type]} mime
                 data (from body)]

             (next-fn
              (-> response
                  (assoc :body data)
                  (assoc-in [:headers "Content-Type"] mime-type)))))))

(defn- wrap-deserialization
  "
  Deserializes body of request with best available media handler.
  "
  [handler media-handlers]

  ^{:mhs media-handlers}
  (fn [{:as   request
       :keys [content-type body]
       :or   {content-type "application/octet-stream"}}]

    (cond
     body (let [mime (lookup-deserializer media-handlers content-type)
                des  (:to mime)
                body (body->str body (:charset mime))]

            (if-some [data (when des (des body))]
              (handler (assoc request :body data))
              (unsupported-media request)))

     :else (handler request))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resource construction

(defn- ne-seq [x] (and (coll? x) (seq x) x))

(defn- wrap
  "
  Conditionally adds middleware logic based upon availability of options.
  "
  [handler middleware options]
  (cond
   (true?  options) (middleware handler)
   (ne-seq options) (middleware handler options)
   options          (middleware handler options)
   :else handler))

(defn- get-sec-fn
  "
  Matches the security handler to the resource.  If it cannot find a match
  it will deny all access to the resource.
  "
  [sec-handlers resource]

  (let [sec-req (:security resource)
        sec-fn  (and sec-req (get sec-handlers sec-req))]

    (when sec-req
      (or sec-fn
          (do
             (pp/write {:error   "Missing Security Handler"
                        :sec-fn  (:security resource)
                        :uris    (:uris resource)
                        :message "All access will be denied."})
             (constantly nil))))))

(defn- gen-trace-id
  "
  Simple, roughly ordered, fast trace id generator.
  "
  []
  (java.util.UUID.
   (System/nanoTime)
   (* (rand-int (Integer/MAX_VALUE))
      (rand-int (Integer/MAX_VALUE)))))

(defn- self-described-handler
  "
  Merges the resource definition from the meta data attached to a handler
  function.
  "
  [{:as resource, r-ops :ops} handler]

  (let [handler-meta              (meta handler)
        {:keys [schema ops desc]} (:tag handler-meta)
        deps (seq (map (comp keyword name)
                       (drop 1 (first (:arglists handler-meta)))))]

    (-> resource
        (merge (when desc   {:desc   desc}))
        (merge (when schema {:schema schema}))
        (merge (when deps   {:deps   deps}))
        (merge (when ops    {:ops (if r-ops
                                    (filter (set ops) r-ops)
                                    ops)})))))

(defn- stack-middleware
  "
  Builds a resource's request & response pipeline as documented in
  make-handler.
  "
  [resource handler {:as    options
                     :keys [sec-handlers, media-handlers, deps,
                            on-request, on-response,
                            schemas, dev-mode]}]

  {:pre [resource handler]}

  (let [resource (self-described-handler resource handler)

        mhs (and media-handlers (concat media-handlers builtin-media-handlers))

        request-fn
        (->
         handler
         ;; Inject dependencies
         (wrap wrap-deps (seq (map deps (:deps resource))))

         ;; Add custom request middleware
         (#(reduce (fn [a m] (m a)) % on-request))

         ;; Request validation middleware
         (wrap wrap-schema-validation (or (get schemas (:schema resource))
                                          (:schema resource)))

         ;; Negotiation middleware
         (wrap wrap-deserialization     mhs)
         (wrap wrap-content-negotiation mhs)
         (wrap wrap-supported-methods (:ops resource))

         ;; Discovery middleware
         (wrap-options resource schemas)

         ;; Security middleware
         (wrap wrap-access-control (filter (comp request-methods key) resource))
         (wrap wrap-authentication (get-sec-fn sec-handlers resource)))

        response-fn
        (->
         identity
         ;; Add custom response middleware
         (#(reduce (fn [a m] (m a)) % on-response))
         ;; Negotiation response middleware
         (wrap wrap-serialization mhs))]

    ^{:resource-definition resource}
    (fn [{:as request :keys [trace-id]}]
      (let [trace-id (or trace-id (gen-trace-id))]

        (-> request
            (assoc :trace-id trace-id, :dev-mode dev-mode)
            (request-fn)
            (assoc :trace-id trace-id, :dev-mode dev-mode)
            (response-fn))))))

(defn- build-resources
  "
  Collects detailed information required for construction of each resource.

  resources is the authoritative source for all uris, security, ops & roles

  handlers is the authoritative source for schema, deps & can further
  restrict available ops beyon definition from resources.

  options contains maps that will be used by resources to lookup sec-handlers,
  deps, on-request, on-response, on-missing & on-error handlers.  Dependencies
  required from handlers will also be resolved through options.
  "
  [resources handlers options]

  (for [{:as rsc, :keys [uris handler]} resources
        :let [hfn (handlers handler)]]

    ;; TODO: if we don't find a resource in handlers, resolve from callers ns
    (do
      (assert (ifn? hfn) (str "Handler function not defined for " rsc))

      {:uris      uris
       :resource (if (ifn? hfn)
                    (stack-middleware rsc hfn options)
                    server-error)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Requirements for binding & router construction

(defn- lift-uris [resources] (for [rsc resources, uri (:uris rsc)]
                               [uri (dissoc rsc :uris)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Router construction

(defn- bindings->wildcard [uri] (str/replace uri #":\w+" "*"))

(defn- table->trie
  "
  Converts the flat table into a routing tree.  If items in table have the
  same path, an AssertionError will be thrown.
  "
  [table]
  (reduce
   (fn [a [ks v]]
     (when-some [x (get-in a ks)]
       (assert (nil? x)
               (format "Routing conflict %s already defined." (apply str ks))))
     (assoc-in a ks v))
   {} table))

(defn- make-routing-trie [table]
  (table->trie (map-first (comp uri->path bindings->wildcard) table)))

(defn- find-route*
  [trie on-missing uri]
  (let [path (uri->path uri)]
    (loop [[h & t] path, trie trie, nf on-missing]

      (let [x (get trie h)              ; trie branch for exact match
            y (or x (get trie "*"))     ; trie branch for wildcard
            r (:resource y)]            ; resource retrieval

        (cond
         ;; searching for best possible route
         (and t y) (recur t y           ; recur with remaining path & trie
                          (if x         ; if this path segment is direct match
                            on-missing  ; set nf to original on-missing
                            (or r nf))) ; otherwise a wildcard overrides nf
         ;; found resource
         r r

         ;; no route found
         :else nf)))))

(defn- make-router
  "
  Takes a tree of path segments to resources and produces a Ring handler
  that uses the :uri to walk the segments to the most specific resource.
  "
  [trie on-missing]
  {:pre [trie on-missing]}

  (let [find-route (memoize (partial find-route* trie on-missing))]

    ^{:routing-trie trie}
    (fn [{:as request :keys [uri]}]
      ((find-route uri) request))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input Validation

(def acl-schema [s/Any])

(defn resource-schema [handlers sec-handlers schemas deps]
  {:uris                      [s/Str]
   :handler                   (apply s/enum (keys handlers))
   (s/optional-key :security) (apply s/enum (keys sec-handlers))
   (s/optional-key :schema)   (apply s/enum (keys schemas))
   (s/optional-key :deps)    [(apply s/enum (keys deps))]
   (s/optional-key :ops)     [(apply s/enum request-methods)]
   (s/optional-key :get)       acl-schema
   (s/optional-key :put)       acl-schema
   (s/optional-key :post)      acl-schema
   (s/optional-key :delete)    acl-schema})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bring it all together

(defmacro resolve-handlers
  "
  When a resources :handler is a symbol, resolves the symbol in the caller's
  *ns* and adding the var into the handlers.

  Returns a map of handlers.
  "
  [resources & [handlers]]
  `(reduce
    (fn [a# rsc#]
      (let [h# (:handler rsc#)]
        (cond
         (a# h#)  a#
         (symbol? h#) (if-some [h-fn# (resolve h#)]
                        (assoc a# h# h-fn#)
                        (throw (RuntimeException.
                                (str "Unable to resolve symbol: " h#)))))))
    (or ~handlers {})
    ~resources))

(defn make-handler*
  "
  Use make-handler.
  "
  [resources handlers {:as    options
                         :keys [sec-handlers, media-handlers, deps,
                                on-missing, on-error,
                                on-request, on-response,
                                schemas, dev-mode]
                         :or   {on-missing not-found}}]

  {:pre [(seq resources)
         (seq handlers)
         (let [rs (resource-schema handlers sec-handlers schemas deps)]
           (s/validate [rs] resources))]}

  (let [on-error (or on-error #(server-error % dev-mode))

        options (assoc options
                  :on-missing on-missing
                  :on-error   on-error)

        router (-> (build-resources resources handlers options)
                   (lift-uris)
                   (attach-bindings)
                   (make-routing-trie)
                   (make-router on-missing))]

    (with-meta
      (wrap-exception-handling router on-error)
      (meta router))))

(defmacro make-handler
  "
  RETURNS a Ring handler with middleware associated based upon
  configuration specified in resources.

  INPUTS:

  resources: a vector of resource definitions, see resource-schema, resources
  is pure data.  Requires uris & handler.

  handlers: resources must refer to a handler, handlers is a map of the
  handler keys to Ring handler functions.

  OPTIONS:

  sec-handlers: resource may reference a security function for use in
  authentication.  sec-handlers is a map from sec-fn-key to an authentication
  function.  Function will return identity on success and nil on failed
  authentication.

  media-handlers: when provided, content negotiation will be handled using the
  functions provided.  media-handlers is a list of handlers.  The first handler
  to support a mime-type will be used for serialization & deserialization of
  that mime-type.  See builtin-media-handlers for examples.

  deps: a map of dependencies that will be injected into requests as required
  by handlers or specified by the resource definitions.

  on-missing: a handler that will be invoked when a uri has no matching route.
  defaults to not-found

  on-error: a handler that will be invoked when any exception is thrown.

  on-request: middleware that will be invoked before the handler.

  on-response: middleware that will be invoked after the handler.

  schemas: resource definitions may reference a schema that will be used to
  validate put, post & patch methods.

                               unsupported
            Auth  ACL  options  op  media  to-clj  schema  on-request  deps
  +=====+    |     |   |        |     |    |       |       |           |   +===+
  |     |---->----->--->-------->----->---->------->------->....>------>-->|   |
  | R A |    |     |   |        |     |            |                       | H |
  | i d |<--401    |   |    <--405    406 or       |                       | a |
  | n a |      <--403  |              415          |                       | n |
  | g p |              |           <--+        <--400                      | d |
  |   t |          <--200 w/                                               | l |
  |   o |           svc desc          <--500                               | e |
  |   r |                                 |                                | r |
  |     |<--------------------------------|---------<------<....<----------|   |
  +=====+                                 |         |           |          +===+
                                       on-error  from-clj  on-response
  "
  ([resources handlers options]
     `(make-handler* ~resources
                     (resolve-handlers ~resources ~handlers)
                     ~options))
  ([resources {:as options :keys [handlers]}]
     `(make-handler* ~resources
                     (resolve-handlers ~resources ~handlers)
                     ~options))
  ([resources]
     `(make-handler* ~resources
                     (resolve-handlers ~resources)
                     {})))

(defn add-prefix
  "This will add a base URI prefix to all resources."
  [prefix resources]
  (for [r resources]
    (into {} (for [[k v] r]
               (if (= :uris k)
                 [k (map #(str "/" prefix "/" %) v)]
                 [k v])))))
