(ns somni.http.mime-test
  (:require [somni.http.mime :refer :all]
            [clojure.test :refer :all]))

(deftest parse-accept-test
  (is (= (vec (map :mime (parse-accept "text/*, text/html, text/html;level=1, */*")))
         ["text/html"
          "text/html"
          "text/*"
          "*/*"])))
