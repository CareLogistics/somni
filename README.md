# somni

### An opinionated yet lightweight services routing library for Clojure Ring.

[![Clojars Project](http://clojars.org/somni/latest-version.svg)](http://clojars.org/somni)

## Why write yet another Ring routing library?
Honestly, it was an accident.  We needed software to create handlers with middleware specified in configuration files.  Once that was written, we noticed that we had written a router.

### Features
- Fast routing using a trie built from URI path segments
- Wildcard routes (e.g., "/foo/*/baz" matches "/foo/bar/baz" & "/foo/123/baz")
- Wildcards can be bound to keywords (e.g., "/foo/:bar/baz" adds :bar to request params)
- Alternates by specifying multiple URIs in the configuration
- Quick feedback from incorrect configuration 
- Separates **security** concerns from handlers
- Full support for Options Http method
- Request negotiation
- Request validation with Prismatic schema
- Simple Dependency injection into requests
- Exceptions wrapped in 500 server errors
- Tracing support

#### There are extension points for customizing:
- authentication functions
- request & response middleware
- serializers & deserializers

## A simple example
```
(require '[carelogistics.somni :as somni])

;; Write a ring handler
(defn hello [request]
  (let [name (get-in request [:params :name] "world")]
    {:status 200, 
     :body (str "Hello " name "!")}))

;; Create a map pointing to all your handlers
(def hello-handlers
  {:hello hello})

;; Configure your resources, this is pure data
(def hello-resources
  [{:uris   ["hello", "hello/:name"], 
    :handler :hello}])

;; Now generate a router with make-handler
(def hello-router (somni/make-handler
                   hello-resources
                   hello-handlers
                   {}))
                   
;; Finally invoke this router as a ring handler
(hello-router {:uri "hello"})     ; ... "hello world!"
(hello-router {:uri "hello/bob"}) ; ... "hello bob!"
```
### License

Copyright &copy; 2014 [CareLogistics, inc.].  
Distributed under the [Eclipse License Version 1.0](http://opensource.org/licenses/eclipse-1.0.php).