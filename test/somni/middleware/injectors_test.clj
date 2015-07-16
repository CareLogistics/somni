(ns somni.middleware.injectors-test
  (:require [somni.middleware.injectors :refer :all]
            [clojure.test :refer :all]))

(defn a [foo bar baz quux]
  (+ foo bar baz quux))

(def ^:private pfm #'somni.middleware.injectors/partial-from-map)

(deftest partial-from-map-test
  (let [f (atom #'a)]
    (is (ifn? (swap! f pfm   {:foo 1})))
    (is (ifn? (swap! f apply {:bar 2})))
    (is (ifn? (swap! f apply {'baz 3})))
    (is (ifn? (swap! f apply {"quux" 4})))
    (is (= 10 (@f)))))

(deftest wrap-deps-test
  (is (= 10 ((inject-deps #'a
                          {:foo 1
                           'bar (context-aware (constantly 2))})
             {:params {"baz" 3} :identity {:quux 4}}))))

(defn b
  ([c a d b] [a b c d])
  ([b a c]   [a b c])
  ([a b]     [a b])
  ([]        :nil))

(def best-match #'somni.middleware.injectors/best-match)
(def partial-from-map #'somni.middleware.injectors/partial-from-map)
(deftest best-match-partial-from-map-test
  (is (= [1 nil 3]
         ((partial-from-map #'b {:a 1, :c 3})))))
