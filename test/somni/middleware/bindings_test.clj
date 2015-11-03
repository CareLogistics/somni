(ns somni.middleware.bindings-test
  (:require [somni.middleware.bindings :refer :all]
            [clojure.test :refer :all]))

(deftest wrap-bindings-test

  (let [handler (attach-bindings
                 identity "/user/:user/:page/:sub-page/?")]

    (is (= (:bindings
            (handler {:uri "/user/e.e.%20cummings/profile/poetic%20works"}))
           {:user "e.e. cummings",
            :page "profile",
            :sub-page "poetic works"})
        "Full bound path test.")

    (is (= (:bindings (handler {:uri "/user/%7B%3Auid%20123%7D/7891"}))
           {:user {:uid 123},
            :page 7891,
            :sub-page nil})
        "Partial bound path test with Clojure data structures"))

  (let [handler (attach-bindings
                  identity "/abc-def/:x/ghi/:y")]
    (is (= (:bindings
             (handler {:uri "/abc-def/1/ghi/2"}))
           {:x 1, :y 2})
        "URI with dash"))

  (let [handler (attach-bindings
                  identity "/abc+def/:x/ghi/:y")]
    (is (= (:bindings
             (handler {:uri "/abc+def/1/ghi/2"}))
           {:x 1, :y 2})
        "URI with plus")))
