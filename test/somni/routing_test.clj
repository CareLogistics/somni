(ns somni.routing-test
  (:require [somni.routing :refer :all]
            [clojure.test :refer :all]))

(deftest bindings->wildcard-test
  (let [expected "/a/*/x"
        actual (bindings->wildcard "/a/:w/x")]

    (is (= actual expected)
        "Convert keyword path segments to wildcards")))

(deftest router-definition-test
  (let [router (atom {})]
    (swap! router add-route '[a b c] 'A)
    (is (= @router '{"a" {"b" {"c" {:somni.routing/h A}}}})
        "Add route to empty router")

    (swap! router add-route '[a b "*" d] 'B)
    (is (= @router '{"a" {"b" {"*" {"d" {:somni.routing/h B}},
                               "c" {:somni.routing/h A}}}})
        "Add route to non-empty router")

    (swap! router add-route '[a b q] "K")
    (is (= @router '{"a" {"b"
                          {"q" {:somni.routing/h "K"},
                           "*" {"d" {:somni.routing/h B}},
                           "c" {:somni.routing/h A}}}})
        "Add route over a wildcard")

    (swap! router add-routes '[[[c] C]
                               [[c d] D]])
    (is (= @router '{"c" {"d" {:somni.routing/h D},
                          :somni.routing/h C},
                     "a" {"b" {"q" {:somni.routing/h "K"},
                               "*" {"d" {:somni.routing/h B}},
                               "c" {:somni.routing/h A}}}})
        "Add routes to non-empty router")

    (is (thrown? clojure.lang.ExceptionInfo (add-route @router '[a b c] 'AB))
        "Adding a route to a pre-existing route is not allowed")))

(deftest remove-routes-test
  (let [router '{c {d {:somni.routing/h D},
                    :somni.routing/h C},
                 a {b {"*" {d {:somni.routing/h B}},
                       c {:somni.routing/h A}}}}]

    (is (= (remove-route router '[a b c])
           '{c {d {:somni.routing/h D},
                :somni.routing/h C},
             a {b {"*" {d {:somni.routing/h B}},
                   c {}}}}))))

(deftest find-handler-test
  (let [router (add-routes {} '[[["a"] A]
                                [["*"] GLOBall]
                                [["a" "b" "*"] AB*]
                                [["a" :user "c"] USERsC]
                                [["p" "*" "q" "*" "d"] PQD]])]

    (is (= (find-handler router ["a"]) 'A))

    (is (= (find-handler router ["z" "r"] 'GLOBall))
        "Trailing GLOB is a greedy match")

    (is (= (find-handler router ["a" "b" "z" "q" 'AB*]))
        "Trailing GLOB test again")

    (is (= (find-handler router ["a" "Andrew" "c" 'USERsC]))
        "Non-trailing GLOB will only match specific segment")

    ;; Additional tests
    (is (= (find-handler router ["a" "Joe" "d"] nil)))
    (is (= (find-handler router ["p" "1" "q" "1" "d"] 'PQD)))
    (is (= (find-handler router ["p" "1" "q" "1" "b"] nil)))
    (is (= (find-handler router ["p" "1" "2" "q" "1" "b"] nil)))))

(deftest router->handler-test
  (let [router (add-routes {} [[["test"]           (fn [_] "test")]
                               [["user" "profile"] (fn [_] "profile")]])
        handler (router->handler router (fn [_] "missing"))]

    (is (thrown? AssertionError (router->handler "NOT A MAP")))
    (is (thrown? AssertionError (router->handler router "NOT A FN")))


    (is (= (handler {:uri "/test"})
           "test"))
    (is (= (handler {:uri "/user/profile"})
           "profile"))
    (is (= (handler {:uri "/a/b/c/d"})
           "missing"))



    ))
