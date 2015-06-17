(ns somni.middleware.auth.injectors-test
  (:require [somni.middleware.injectors :refer :all]
            [clojure.test :refer :all]))

(defn a [foo bar baz quux] (+ foo bar baz quux))

(deftest partial-from-map-test
  (let [f (atom #'a)]
    (is (ifn? (swap! f partial-from-map {:foo 1})))
    (is (ifn? (swap! f apply {:bar 2})))
    (is (ifn? (swap! f apply {'baz 3})))
    (is (ifn? (swap! f apply {"quux" 4})))
    (is (= 10 (@f)))))

(deftest request->deps-test
  (let [request->deps #'somni.injectors/request->deps]
    (is (= (request->deps {:body 123, :headers {}, :identity {}})
           {:request {:body 123, :headers {}, :identity {}}
            :body 123,
            :headers {},
            :identity {}}))))

(def deps {:foo 1 'bar 2 "quux" 3 :baz 4})
(deftest wrap-deps-test
  (let [a-with-deps (wrap-deps* #'a deps)]
    (is (= 10 (a-with-deps {})))))
