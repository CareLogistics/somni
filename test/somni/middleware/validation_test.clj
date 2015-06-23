(ns somni.middleware.validation-test
  (:require [somni.middleware.validation :refer :all]
            [clojure.test :refer :all]
            [schema.core :as s]))

(def ^:private +schema+ {:user s/Str
               :action s/Keyword})

(def ^:private h (wrap-request-validation identity +schema+))

(def ^:private req1 {:request-method :put
                     :body {:user "bob", :action :rock-on}})

(def ^:private invalid-req {:request-method :post
                            :body {:user "bob", :action "bad action"}})

(deftest wrap-schema-validation-test
  (is (= (h req1) req1)
      "Validation lets good data through")

  (is ((h invalid-req) :errors)
      "Validation stops bad data"))
