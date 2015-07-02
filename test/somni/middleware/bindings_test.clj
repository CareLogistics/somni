(ns somni.middleware.bindings-test
  (:require [somni.middleware.bindings :refer :all]
            [clojure.test :refer :all]))

(deftest wrap-bindings-test

  (let [handler (attach-bindings-to-request-params
                 identity "/user/:user/:page/:sub-page")]

    (is (= (:params (handler {:uri "/user/e.e.%20cummings/profile/poetic%20works"}))
           {:user "e.e. cummings",
            :page "profile",
            :sub-page "poetic works"})
        "Full bound path test.")

    (is (= (:params (handler {:uri "/user/%7B%3Auid%20123%7D/7891"}))
           {:user {:uid 123},
            :page 7891,
            :sub-page nil})
        "Partial bound path test with Clojure data structures")))
