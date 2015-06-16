(ns somni.injectors-test
  (:require [somni.injectors :refer :all]
            [clojure.test :refer :all]))

(defn a [foo bar baz quux] (+ foo bar baz quux))

(deftest test-invoke-with-map
  (let [f (atom #'a)]
    (is (ifn? (swap! f invoke-with-map {:foo 1})))
    (is (ifn? (swap! f apply {:bar 2})))
    (is (ifn? (swap! f apply {'baz 3})))
    (is (= 10 (swap! f apply {"quux" 4})))))
