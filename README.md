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

##Performance
```
Time for 1000000 matches using Compojure routes
"Elapsed time: 6685.902 msecs"
Time for 1000000 matches using somni routes
"Elapsed time: 3310.259 msecs"
```
Compojures routing is O(n).  Somni uses a routing tree to achieve O(log n) performance.  For a few routes, there's little difference.  The test above is with 28 URIs.  The difference becomes more and more pronounced as routes are added.

```
Time for 1000000 matches to 10000 somni routes
"Elapsed time: 3908.063 msecs"
```
Somni's routing performance is faster than other Ring routing routing libraries I've tested - currently: gudu, bidi & compojure.  The above test generates 10000 URI routes with between 2 and 5 URI path segments.  The difference between 28 & 10k routes is minimal.<p>

There are additional tests comparing compojure and somni performance in perf-tests.clj.

## License

Copyright &copy; CareLogistics, inc. 2014-.<br>
Released under the [Eclipse License Version 1.0](http://opensource.org/licenses/eclipse-1.0.php), the same license as Clojure.