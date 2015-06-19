(ns somni.middleware.bindings-test
  (:require [somni.middleware.bindings :refer :all]
            [clojure.test :refer :all]))

(deftest wrap-bindings-test

  (let [handler (wrap-bindings identity "/user/:user/:page/:sub-page")]

    (is (= (get-in (handler {:uri "/user/bob/profile/contacts"})
                   [:params :sub-page])
           "contacts")
        "Fully bound path works")

    (is (= (get-in (handler {:uri "/user/pete/wall"})
                   [:params :sub-page])
           nil)
        "URI without matching segment sets param to nil")))
