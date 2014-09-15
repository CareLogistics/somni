;;; Copyright (c) Care Logistics, inc. All rights reserved.
;;; The use and distribution terms for this software are covered by the
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.html at the root of this
;;; distribution.
;;; By using this software in any fashion, you are agreeing to be bound by
;;; the terms of this license.
;;; You must not remove this notice, or any other, from this software.

(ns carelogistics.somni-test
  (:require [clojure.test :as test :refer :all]
            [carelogistics.somni :refer :all]
            [schema.core :as schema]))

(defn OK
  "OK produces a function that constantly returns a single value."
  [body]
  (fn [_] {:status 200,
          :content-type "application/text"
          :body (str body)}))

(def quux
  ^{:desc "This is a service that returns a happy message when Somni works."
    :ops [:post]
    :deps [:abc]
    :schema {:x schema/Str, :y schema/Any}}
  (fn [{:keys [abc]}]
    (when abc {:status 200, :body "everything is just fine"})))

(def sample-handlers
  {:foo    (OK "foo")
   :foobar (OK "foobar")
   :foobaz (OK "foobaZ")
   :in-mw  (fn [r] {:status 200, :body (str (:in-mw r))})
   :alts   (OK "alternate paths work")
   :deps   (fn [r] {:status 200, :body (:abc r "eep")})
   :err    (fn [_] (throw (Exception. "hahaha")))
   :full   (OK "lots of stuff going on here")
   :pbinds (fn [{{:keys [alpha beta]} :params}]
             {:status 200 :body [alpha beta]})
   :quux   quux})

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

(defn test-handler []
  (make-handler
   sample-resource-table
   sample-handlers
   {:sec-handlers sample-sec-handlers
    :schemas      sample-schemas
    :on-request   sample-in-middlware
    :on-response  sample-out-middlware
    :deps         {:abc 1234}}))

(defn- conflicting-routes []
  (make-handler [{:uris ["foo/bar" "foo/bar"], :handler :boom}]
                {:boom (OK "no way")}
                {}))

(deftest unit-tests
  (let [r (test-handler)]

    (testing "Testing Routing"
      (is (= "foo"    (:body (r {:uri "/foo"}))))
      (is (= "foobar" (:body (r {:uri "/foo/bar"}))))
      (is (= "foobaZ" (:body (r {:uri "/foo/baz/123"}))))
      (is (= "foobaZ" (:body (r {:uri "/foo/baz/quux/123"})))))

    (testing "Routes with binding wildcard patterns"
      (is (= ["ana" "ab"] (:body (r {:uri "/ana/ab"}))))

      (is (= ["ana" "ab"] (:body (r {:uri "/foo/ab/ana"}))))

      (is (:trace-id (r {:uri "foo/xyz/def"}))
          "Binding middleware is breaking pipeline"))

    (testing "Basic features"

      (is (:trace-id (r {:uri "/foo"})) "Missing trace-id")

      (is (:out-mw (r {:uri "/foo"}))   "Response middleware failed")

      (is (= "works"
             (:body (r {:uri "/in-mw",
                        :body "123"}))) "Request middleware failed")

      (is (= 1234 (:body  (r {:uri "/deps"}))) "Dependency injection failed")

      (is (= 500 (:status  (r {:uri "/err"}))) "Exception encapsulation failed")

      (is (= (:body (r {:uri "/A"}))
             (:body (r {:uri "/B"}))) "Alternate routes failed"))

    (testing "Request errors features"

      (is (= 401 (:status (r {:uri "/full", :body "bar"})))
          "Authentication middleware missing")

      (is (= 403 (:status (r {:uri "/full",
                              :headers {"Auth" "foo"},
                              :request-method :get})))
          "ACL middleware missing")

      (is (= 405 (:status (r {:uri "/full", :headers {"Auth" "foo"}})))
          "Supported methods check missing")

      (is (= 400
             (:status (r {:uri "/full",
                          :headers {"Auth" "jwt"},
                          :body 99,
                          :request-method :put})))
          "Schema validation missing")

      (is (= 200
             (:status (r {:uri "/full",
                          :headers {"Auth" "jwt"},
                          :body "awesome",
                          :request-method :put})))
          "Something broke, look at the stack trace"))

    (testing "Self described request handlers"

      (is (= 405 (:status (r {:uri "quux"})))
          "Supported methods check not merged in")

      (is (= 400 (:status (r {:uri "quux",
                              :request-method :post
                              :body {:x 99, :y 99}})))
          "Schema validation not merged in")

      (is (= 200 (:status (r {:uri "quux",
                              :request-method :post
                              :body {:x "99", :y 99}})))
          "Something broke, look at the stack trace"))

    (testing "Content negotiation tests")

    (testing "Options returns documentation")

    (testing "Bidirectional routing features")

    (testing "Assertion tests"
      ;; TODO: add tests for other assertions that are produced
      (is (thrown? java.lang.AssertionError (conflicting-routes))))))

(def hello-handlers
  {:hello (fn [request]
            (let [name (get-in request [:params :name] "world")]
              {:status 200
               :body (str "Hello " name "!")}))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Hello world example

(require '[carelogistics.somni :as somni])

(def hello-resources
  [{:uris ["hello" "hello/:name"], :handler :hello}])

(def hello-router (somni/make-handler
                   hello-resources
                   hello-handlers
                   {}))

(prn (hello-router {:uri "hello"})
     (hello-router {:uri "hello/bob"}))

"(-: begin the testing :-)"
