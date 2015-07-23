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

(defn ^{:consumes ["application/eliza"]}
  hello-api [] {:msg "Hello"})

(def Hello {:msg s/Str})

(def sample-resources
  [{:uri "/user/:id"
    :post #'sample-fn}

   {:uri "/hello/api"
    :get #'hello-api
    :put #'hello-api
    :doc "Says hello"
    :response Hello
    :consumes ["overridden/by-meta"]
    :produces ["application/vnd+excel"]}])

(defonce validator
  (scjsv/validator
   (slurp "https://raw.githubusercontent.com/swagger-api/swagger-spec/master/schemas/v2.0/schema.json")))

(deftest test-swagger
  (let [swag (resources->swagger sample-resources)]
    (is (nil? (validator (rs/swagger-json swag))))

    (is (= (get-in swag [:paths "/user/:id" :post :parameters :body])
           User))

    (is (= (get-in swag [:paths "/hello/api" :get :responses 200 :schema])
           Hello))))
