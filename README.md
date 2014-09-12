# somni

An opinionated yet lightweight services routing library for Clojure Ring.

## Releases and Depency Information 

[![Clojars Project](http://clojars.org/somni/latest-version.svg)](http://clojars.org/somni)

Depends upon Clojure and prismatic/schema.

## Description and Usage

### This is the pipeline the library builds for its resource handlers:
```
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
                                       on-error  from-clj  on-response ```

### The obligatory hello world application.
```clj
(require '[carelogistics.somni :as somni])

;; Map of resource handlers 
(def hello-handlers
  {:hello (fn [request]
            (let [name (get-in request [:params :name] "world")]
              {:status 200
               :body (str "Hello " name "!")}))})

;; List of resource definitions
(def hello-resources
  [{:uris ["hello", "hello/:name"], :handler :hello}])

;; Now generate a router with make-handler
(def hello-router (somni/make-handler
                   hello-resources
                   hello-handlers
                   {}))
                   
;; Finally invoke this router as a ring handler
(hello-router {:uri "hello"})
#=> {:dev-mode nil, 
     :trace-id #uuid "13935054-5e08-0cb0-009a-d3bb3f7843fa",
     :status 200, 
     :body "Hello world!"}

(hello-router {:uri "hello/bob"})
#=> 
```

### License

Copyright &copy; 2014 [CareLogistics, inc.].  
Distributed under the [Eclipse License Version 1.0](http://opensource.org/licenses/eclipse-1.0.php).
