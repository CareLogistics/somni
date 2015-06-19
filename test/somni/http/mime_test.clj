(ns somni.http.mime-test
  (:require [somni.http.mime :refer :all]
            [clojure.test :refer :all]))

(deftest parse-accept-test
  (is (= (vec (parse-accept "text/*, text/html, text/html;level=1, */*"))
         [{:charset "UTF-8", :level "1", :q "1.0", :group "text", :media :html}
          {:charset "UTF-8", :q "1.0", :group "text", :media :html}
          {:charset "UTF-8", :q "1.0", :group "text", :media :*}
          {:charset "UTF-8", :q "1.0", :group "*", :media :*}])))
