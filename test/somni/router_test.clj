(ns somni.router-test
  (:require [somni.router :refer :all]
            [clojure.test :refer :all]))

(deftest bindings->wildcard-test
  (is
   (= (bindings->wildcard "/a/:w/x")
      "/a/*/x")
   "Convert keyword path segments to wildcards"))

(deftest router-definition-test
  (let [router (atom {})]
    (is
     (= (swap! router add-route '[a b c] 'A)
        '{"a" {"b" {"c" {:somni.routing/h A}}}})
     "Add route to empty router")

    (is
     (= (swap! router add-route '[a b "*" d] 'B)
        '{"a" {"b" {"*" {"d" {:somni.routing/h B}},
                    "c" {:somni.routing/h A}}}})
     "Add route to non-empty router")

    (is
     (= (swap! router add-route '[a b q] "K")
        '{"a" {"b" {"q" {:somni.routing/h "K"},
                    "*" {"d" {:somni.routing/h B}},
                    "c" {:somni.routing/h A}}}})
     "Add route over a wildcard")

    (is
     (= (swap! router add-routes '[[[c] C]
                                   [[c d] D]])
        '{"c" {"d" {:somni.routing/h D},
               :somni.routing/h C},
          "a" {"b" {"q" {:somni.routing/h "K"},
                    "*" {"d" {:somni.routing/h B}},
                    "c" {:somni.routing/h A}}}})
     "Add routes to non-empty router")

    (is
     (thrown? clojure.lang.ExceptionInfo
              (add-route @router '[a b c] 'AB))
     "Adding a route to a pre-existing route is not allowed")))

(deftest remove-routes-test
  (let [router '{c {d {:somni.routing/h D},
                    :somni.routing/h C},
                 a {b {"*" {d {:somni.routing/h B}},
                       c {:somni.routing/h A}}}}]

    (is
     (= (remove-route router '[a b c])
        '{c {d {:somni.routing/h D},
             :somni.routing/h C},
          a {b {"*" {d {:somni.routing/h B}},
                c {}}}})
     "remove-route removes handler from routing path")))

(deftest find-handler-test
  (let [routes '[[["site-map"] site-map-handler]
                      [["*"] index-page]
                      [["site-map" "seo" "details"] seo-details]
                      [["site-map" :user "c"] users-index]
                      [["page" "*" "visitor" "*" "ad-metrics"] page-visitor-ad-impressions]]
        router (add-routes {} routes)]

    (is
     (= (find-handler router ["site-map"]) 'site-map-handler)
     "Simple top level route")

    (is
     (= (find-handler router ["site-map" "seo" "details" 'seo-details]))
     "Simple multi level route")

    (is
     (= (find-handler router ["z" "r"] 'index-page))
     "Trailing GLOB is a greedy match")

    (is
     (= (find-handler router ["site-map" "Andrew" "c" 'users-index]))
     "Non-trailing GLOB will only match specific path segment")

    (is
     (= (find-handler router ["page" "1" "visitor" "1" "ad-metrics"]
                      'page-visitor-ad-impressions))
     "Multiple globs will match specific segments")

    (is (= (find-handler router ["site-map" "Joe" "ad-metrics"] nil)))
    (is (= (find-handler router ["page" "1" "visitor" "1" "b"] nil)))
    (is (= (find-handler router ["page" "1" "2" "visitor" "1" "b"] nil)))))

(deftest router->handler-test
  (let [routes [[["test"]           (fn [_] "test")]
                [["user" "profile"] (fn [_] "profile")]]
        router  (add-routes {} routes)
        handler (router->handler router (fn [_] "missing"))]

    (is (= (handler {:uri "/test"})         "test"))
    (is (= (handler {:uri "/user/profile"}) "profile"))
    (is (= (handler {:uri "/a/b/c/d"})      "missing"))))
