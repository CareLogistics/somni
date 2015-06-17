(ns somni.middleware.auth.injectors-test
  (:require [somni.middleware.injectors :refer :all]
            [clojure.test :refer :all]))

(defn a [foo bar baz quux] (+ foo bar baz quux))

(def ^:private pfm #'somni.middleware.injectors/partial-from-map)

(deftest partial-from-map-test
  (let [f (atom #'a)]
    (is (ifn? (swap! f pfm   {:foo 1})))
    (is (ifn? (swap! f apply {:bar 2})))
    (is (ifn? (swap! f apply {'baz 3})))
    (is (ifn? (swap! f apply {"quux" 4})))
    (is (= 10 (@f)))))

(deftest request->deps-test
  (let [request->deps #'somni.middleware.injectors/request->deps]
    (is (= (request->deps {:body 123,
                           :headers {"a" "b"},
                           :params {:visit-id "x"}
                           :identity {:u 1 :r [:x :y]}})
           {:visit-id "x",
            :u 1,
            :r [:x :y],
            :headers {"a" "b"},
            :body 123,
            :request {:identity {:r [:x :y], :u 1},
                      :params {:visit-id "x"},
                      :headers {"a" "b"},
                      :body 123}}))))

(def deps {:foo 1 'bar 2 "quux" 3 :baz 4})
(deftest wrap-deps-test
  (let [a-with-deps (wrap-deps* #'a deps)]
    (is (= 10 (a-with-deps {})))))
