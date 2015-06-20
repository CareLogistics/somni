(ns somni.marshalling.deserializers-test
  (:require [somni.marshalling.deserializers :refer :all]
            [clojure.test :refer :all]))

(def test-body "{a 1 b 2}")

(deftest deserializable?-test
  (is (not (deserializable? {}))
      "Can't deserialize when there's no content-type")

  (is (not (deserializable? {:content-type "application/edn"}))
      "Can't deserialize when there's no body")

  (is (deserializable? {:content-type "application/edn"
                        :body test-body})))

(deftest deserializable-test
  (is (= test-body
         (:body (deserialize {:body test-body}))))

  (is (= '{a 1 b 2}
         (:body (deserialize {:content-type "application/edn"
                              :body test-body}))))

    (is (= {:a 1 :b 2}
         (:body (deserialize {:content-type "application/x-www-form-urlencoded"
                              :body "a=1&b=2"})))))
