(ns somni.middleware.to-ring-test
  (:require [somni.middleware.to-ring :refer :all]
            [clojure.test :refer :all]))

(deftest wrap-->ring-test
  (is (= 200 (:status ((wrap-->ring identity) "foo"))))
  (is (= 201 (:status ((wrap-->ring (fn [_] {:status 201})) {})))))
