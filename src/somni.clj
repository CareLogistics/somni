;;; Copyright (c) Care Logistics, inc. All rights reserved.
;;; The use and distribution terms for this software are covered by the
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.html at the root of this
;;; distribution.
;;; By using this software in any fashion, you are agreeing to be bound by
;;; the terms of this license.
;;; You must not remove this notice, or any other, from this software.

(ns ^{:author "Andrew Garman"}
  somni
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

#_
(defn wrap-supported-methods
  ;; TODO - route exists but operation doesn't
  nil)


(defn wrap-options
  ;;
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Requirements for binding & router construction

(defn- lift-uris [resources] (for [rsc resources, uri (:uris rsc)]
                               [uri (dissoc rsc :uris)]))


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
                       :keys [ deps,
                              on-missing, on-error,
                              on-request, on-response,
                               dev-mode]
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
      (wrap-uncaught-exceptions router on-error)
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
  |   o |           svc desc      <--500                                   | e |
  |   r |                             |                                    | r |
  |     |<----------------------------|---------<------<....<---------<----|   |
  +=====+                             |         |           |         |    +===+
                                   on-error  from-clj  on-response  ring<-
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
