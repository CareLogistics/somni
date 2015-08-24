(ns somni.middleware.to-ring-test
  (:require [somni.middleware.to-ring :refer :all]
            [clojure.test :refer :all]))

(deftest wrap-->ring-test
  (is (= ((wrap-response-as-ring (fn [_] {:status 111 :body "untouched"}))
          {})
         {:status 111 :body "untouched"})
      "Use response unchanged if ring response regardless the request")

  (let [nil-handler (wrap-response-as-ring (constantly nil))]
    (is (= (:status (nil-handler {:request-method :get}))
           404)
        "nil returned from handler on a get request produces 404")

    (is (= (:status (nil-handler {:request-method :all-other}))
           204)
        "nil returned from other handler types produces 204"))

  (let [t-handler (wrap-response-as-ring (constantly 't))]
    (is (= (t-handler {})
           (t-handler {:request-method :get})
           (t-handler {:request-method :delete})
           {:status 200 :body 't :headers {"Etag" (str (hash 't))}})
        "a handler that returns a non-nil value results in 200")))
