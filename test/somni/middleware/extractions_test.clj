(ns somni.middleware.extractions-test
  (:require [somni.middleware.extractions :refer :all]
            [clojure.test :refer :all]))

(deftest wrap-extractions-test
  (let [h (wrap-extractions identity "?")]

    (is (= (h {:request-method :get
               :uri "foo/bar"})
           nil)
        "extraction that doesn't match")

    (is (= (h {:uri "foo/bar"})
           {:uri "foo/bar"})
        "extraction only affects gets")

    (is (= (h {:request-method :get
               :uri "foo/bar"
               :foo {:bar [1 2 3 4]}})
           [[1 2 3 4]])
        "extraction works on maps")

    (is (= (vec (h {:request-method :get
                    :uri "foo/bar/1"
                    :foo [{:bar 1 :data {:d 7}}
                          {:bar 2 :data {:c 5}}
                          {:bar 3 :data {:b 3}}
                          {:bar 4 :data {:a 1}}]}))
           [ {:bar 1 :data {:d 7}}])
        "extraction works on collections of maps")

    (is (= (h {:request-method :get
               :uri "foo/bar/1/data/d/7"
               :foo [{:bar 1 :data [{:d 7}]}
                     {:bar 2 :data {:c 5}}
                     {:bar 3 :data {:b 3}}
                     {:bar 4 :data {:a 1}}]})
           [{:d 7}])
        "extraction is recursive")))
