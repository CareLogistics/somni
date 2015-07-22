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

(defn hello-api [] {:msg "Hello"})

(def sample-resources
  [{:uri "/user/:id"
    :post #'sample-fn}

   {:uri "/hello/api"
    :get #'hello-api
    :doc "Says hello"
    :response {:msg s/Str}
    :consumes ["fat/penguins" "chubby/baby+seals"]
    :produces ["larger/orca" "rounded/sharks"]}])

(defonce validator
  (scjsv/validator
   (slurp "https://raw.githubusercontent.com/swagger-api/swagger-spec/master/schemas/v2.0/schema.json")))

(deftest test-swagger
  (is (nil? (-> (resources->swagger sample-resources)
                (rs/swagger-json)
                (validator)))))
