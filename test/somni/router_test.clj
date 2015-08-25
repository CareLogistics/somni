(ns somni.router-test
  (:require [somni.router :refer :all]
            [clojure.test :refer :all]))

(deftest bindings->wildcard-test
  (is
   (= (wildcards->globs "/a/:w-!-123-souffle/?/x/?")
      "/a/*/?/x/*")
   "Convert keyword path segments to wildcards"))

(deftest router-definition-test
  (let [router (atom {})]
    (is
     (= (swap! router add-route :get "a/b/c" 'A)
        '{"a" {"b" {"c" {:get A}}}})
     "Add route to empty router")

    (is
     (= (swap! router add-route :get "a/b/:kw/d" 'B)
        '{"a" {"b" {"*" {"d" {:get B}},
                    "c" {:get A}}}})
     "Add route to non-empty router")

    (is
     (= (swap! router add-route :get "a/b/q" "K")
        '{"a" {"b" {"q" {:get "K"},
                    "*" {"d" {:get B}},
                    "c" {:get A}}}})
     "Add route over a wildcard")

    (is
     (= (swap! router add-routes '[[:get "c" C]
                                   [:get "c/d" D]])
        '{"c" {"d" {:get D},
               :get C},
          "a" {"b" {"q" {:get "K"},
                    "*" {"d" {:get B}},
                    "c" {:get A}}}})
     "Add routes to non-empty router")

    (is
     (thrown? clojure.lang.ExceptionInfo
              (add-route @router :get "a/b/c" 'AB))
     "Adding a route to a pre-existing route is not allowed")

    (is
     (= (swap! router remove-route :get "a/b/c")
        '{"c" {"d" {:get D},
              :get C},
         "a" {"b" {"q"
                   {:get "K"},
                   "*" {"d" {:get B}},
                   "c" {}}}})
     "Remove removes a route")))

(deftest find-handler-test
  (let [routes '[[:get "/site-map" site-map-handler]
                 [:get "/*" index-page]
                 [:get "/site-map/seo/details" seo-details]
                 [:get "/site-map/:user/c" users-index]
                 [:get "/page/*/visitor/*/ad-metrics" page-visitor-ad-impressions]]
        router (add-routes {} routes)]

    (is
     (= (find-handler router :get ["site-map"])
        'site-map-handler)
     "Simple top level route")

    (is
     (= (find-handler router :get ["site-map" "seo" "details"])
        'seo-details)
     "Simple multi level route")

    (is
     (= (find-handler router :get ["z" "r"])
        'index-page)
     "Trailing GLOB is a matches unmatched routes greedily")

    (is
     (= (find-handler router :get ["site-map" "Andrew" "c"])
        'users-index)
     "Non-trailing GLOB will only match specific path segment")

    (is
     (= (find-handler router :get ["page" "1" "visitor" "1" "ad-metrics"])
        'page-visitor-ad-impressions)
     "Multiple globs will match specific segments")

    (is (find-handler router :get ["site-map" "Joe" "ad-metrics"]))
    (is (find-handler router :get ["page" "1" "visitor" "1" "b"]))
    (is (find-handler router :get ["page" "1" "2" "visitor" "1" "b"]))))

(deftest router->handler-test
  (let [routes [[:get ["test"]           (fn [_] "test")]
                [:put ["user" "profile"] (fn [_] "profile")]]
        router  (add-routes {} routes)
        handler (router->handler router (fn [_] "missing"))]


    (is (= (handler {:uri "/test", :request-method :get})
           "test"))

    (is (= (handler {:uri "/user/profile", :request-method :put})
           "profile"))

    (is (= (handler {:uri "/a/b/c/d", :request-method :get})
           "missing"))

    (is (= (handler {:uri "/test", :request-method :put})
           {:status 405, :body "Unsupported HTTP method"}))))
