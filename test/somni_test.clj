;;; Copyright (c) Care Logistics, inc. All rights reserved.
;;; The use and distribution terms for this software are covered by the
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.html at the root of this
;;; distribution.
;;; By using this software in any fashion, you are agreeing to be bound by
;;; the terms of this license.
;;; You must not remove this notice, or any other, from this software.

(ns somni-test
  (:require [clojure.test :as test :refer :all]
            [somni :refer :all]
            [schema.core :as schema]))

(defn OK
  "OK produces a function that constantly returns a single value."
  [body]
  (fn [_] {:status 200,
          :headers {"Content-Type" "application/text"}
          :body (str body)}))

(def +quux+
  {:desc "This is a service that returns a happy message when Somni works."
   :ops [:post]
   :schema {:x schema/Str, :y schema/Any}})

(defn ^+quux+ quux
  [r abc]
  (when abc {:status 200, :body "everything is just fine"}))

(def sample-handlers
  {:foo    (OK "foo")
   :foobar (OK "foobar")
   :foobaz (OK "foobaZ")
   :full   (OK "lots of stuff going on here")

   :alts   (OK "alternate paths work")
   :in-mw  (fn [r] {:status 200, :body (str (:in-mw r))})
   :deps   (fn [r abc] {:status 200, :body (or abc "eep")})
   :err    (fn [_] (throw (Exception. "hahaha")))
   :err2   (fn [_] (throw (ex-info "missing dep" {:status 555})))

   :pbinds (fn [{{:keys [alpha beta]} :params}]
             {:status 200 :body [alpha beta]})
   :quux   #'quux})

(def sample-sec-handlers
  {:jwt (fn [{:keys [headers]}]
          (condp = (get headers "Auth")
            "jwt" {:id "me", :role :patient}
            "foo" {:id "ze", :role :nurse}
            nil))})

(def sample-in-middlware
  [(fn [handler]
     (fn [request]
       (handler (assoc request :in-mw "works"))))])

(def sample-out-middlware
  [(fn [handler]
     (fn [response]
       (handler (assoc response :out-mw "works"))))])

(def sample-resource-table
  [{:uris ["quux"], :handler :quux}
   {:uris ["/foo"], :handler :foo}
   {:uris ["/foo/bar"], :handler :foobar}
   {:uris ["/foo/baz/*"], :handler :foobaz}
   {:uris ["in-mw"], :handler :in-mw}
   {:uris ["err"], :handler :err}
   {:uris ["err2"], :handler :err2}
   {:uris ["A", "B"], :handler :alts}
   {:uris ["/:alpha/:beta" "/foo/:beta/:alpha"], :handler :pbinds}
   {:uris ["deps"], :handler :deps, :deps [:abc]}
   {:uris ["full"]
    :security :jwt
    :ops [:get, :put]
    :get [:patient, :family]
    :put [:patient]
    :schema :full
    :handler :full}])

(def sample-schemas {:full schema/Str})

(def test-handler (make-handler
                   sample-resource-table
                   sample-handlers
                   {:sec-handlers sample-sec-handlers
                    :schemas      sample-schemas
                    :on-request   sample-in-middlware
                    :on-response  sample-out-middlware
                    :media-handlers []
                    :deps         {:abc 1234}}))

(deftest basic-routing-tests
  (is (= "foo"    (:body (test-handler {:uri "/foo"}))))
  (is (= "foobar" (:body (test-handler {:uri "/foo/bar"}))))
  (is (= "foobaZ" (:body (test-handler {:uri "/foo/baz/123"})))))

(deftest wildcard-routing-tests
  (is (= ["ana" "ab"] (:body (test-handler {:uri "/ana/ab"}))))
  (is (= ["ana" "ab"] (:body (test-handler {:uri "/foo/ab/ana"}))))
  (is (get-in (test-handler {:uri "foo/xyz/def"}) [:headers :trace-id] ) "maintains trace"))

(deftest features-tests
  (is (get-in (test-handler {:uri "/foo"}) [:headers :trace-id]) "Missing trace-id")

  (is (:out-mw (test-handler {:uri "/foo"})) "Response middleware failed")

  (is (= "works" (:body (test-handler {:uri "/in-mw"})))
      "Request middleware failed")

  (is (= 1234 (:body  (test-handler {:uri "/deps"})))
      "Dependency injection failed")

  (is (= 500 (:status  (test-handler {:uri "/err"})))
      "Exception encapsulation failed")

  (is (= 555 (:status  (test-handler {:uri "/err2"})))
      "Failed to set status correctly")

  (is (= (:body (test-handler {:uri "/A"}))
         (:body (test-handler {:uri "/B"}))) "Alternate routes failed"))

(deftest content-negotiation-tests
  (is (= "application/text"
         (get-in (test-handler {:uri "/foo/baz/quux/123"})
                 [:headers "Content-Type"])))

  (is (= 415
         (:status (test-handler {:uri "/full",
                                 :headers {"Auth" "jwt"},
                                 :body 99,
                                 :request-method :put})))
      "Not checking content-type")

  (is (= 406 (:status (test-handler {:uri "/full",
                                     :headers {"Auth" "jwt"
                                               "Accept" "xxx/yyy"},
                                     :request-method :put})))
      "Failing to fail on accept header"))

(deftest pipeline-features-tests
  (is (= 401 (:status (test-handler {:uri "/full", :body "bar"})))
      "Authentication middleware missing")

  (is (= 403 (:status (test-handler {:uri "/full",
                                     :headers {"Auth" "foo"},
                                     :request-method :get})))
      "ACL middleware missing")

  (is (= 405 (:status (test-handler {:uri "/full", :headers {"Auth" "foo"}})))
      "Supported methods check missing")

  (is (= 400
         (:status (test-handler {:uri "/full",
                                 :headers {"Auth" "jwt"},
                                 :body (pr-str 99)
                                 :content-type "application/edn"
                                 :request-method :put})))
      "Schema validation missing")

  (is (= 200
         (:status (test-handler {:uri "/full",
                                 :headers {"Auth" "jwt"},
                                 :content-type "application/edn"
                                 :body (pr-str "awesome"),
                                 :request-method :put})))
      "Something broke, look at the stack trace"))

(deftest self-described-handler-tests
  (is (= 405 (:status (test-handler {:uri "quux"})))
      "Supported methods check not merged in")

  (is (= 400 (:status (test-handler {:uri "quux",
                                     :request-method :post
                                     :content-type "application/edn"
                                     :body (pr-str {:x 99, :y 99})})))
      "Schema validation not merged in")

  (is (= 200 (:status (test-handler {:uri "quux",
                                     :request-method :post
                                     :content-type "application/edn"
                                     :body (pr-str {:x "99", :y 99})})))
      "Something broke, look at the stack trace"))

(deftest options-documentation-tests)

(deftest bidirection-features-tests)

(defn- conflicting-routes []
  (make-handler [{:uris ["foo/bar" "foo/bar"], :handler :boom}]
                {:boom (OK "no way")}
                {}))

(deftest assertion-failure-tests
  ;; TODO: add tests for other assertions that are produced
  (is (thrown? java.lang.AssertionError (conflicting-routes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Hello world example

(require 'somni)

(def hello-handlers
  {:hello (fn [request]
            (let [name (get-in request [:params :name] "world")]
              {:status 200
               :body (str "Hello " name "!")}))})

(def hello-resources
  [{:uris ["hello" "hello/:name"], :handler :hello}])

(def hello-router (somni/make-handler
                   hello-resources
                   hello-handlers
                   {}))

(prn (hello-router {:uri "hello"}))
(prn (hello-router {:uri "hello/bob"}))

"(-: begin the testing :-)"
