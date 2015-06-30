(ns somni.http.mime-test
  (:require [somni.http.mime :refer :all]
            [clojure.test :refer :all]))

(deftest parse-accept-test
  (is (= (vec (map :media-type (parse-accept "text/*, text/html, text/xhtml;level=1, */*")))
         ["text/xhtml"
          "text/html"
          "text/*"
          "*/*"])))
