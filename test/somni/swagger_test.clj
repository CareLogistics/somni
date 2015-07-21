(ns somni.swagger-test
  (:require [clojure.test :refer :all]
            [ring.swagger.swagger2 :as rs]
            [schema.core :as s]
            [scjsv.core :as scjsv]
            [somni.swagger :refer :all]))

(s/defschema User
  {:id s/Str,
   :name s/Str,
   :address {:street s/Str
             :city (s/enum :foo :bar)}})

(defn ^{:schema User,
        :consumes ["app/stuff"],
        :produces ["app/fluff"]}
  ^User sample-fn
  "Doc for sample-fn"
  [{:keys [id name address] :as user}])

(s/defschema Hello {:msg s/Str})

(defn ^Hello hello-api "Hello world" [] "Hello")

(def sample-resources
  [{:uri "/user/:id"
    :post #'sample-fn}

   {:uri "/hello/api"
    :get #'hello-api}])

(defonce validator
  (scjsv/validator
   (slurp "https://raw.githubusercontent.com/reverb/swagger-spec/master/schemas/v2.0/schema.json")))

(deftest test-swagger
  (is (nil? (-> (resources->swagger sample-resources)
                (rs/swagger-json)
                (validator)))))
