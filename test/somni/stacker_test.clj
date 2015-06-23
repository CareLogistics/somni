(ns somni.stacker-test
  (:require [somni.stacker :refer :all]
            [clojure.test :refer :all]
            [schema.core :as s]
            [somni.middleware.access-control :refer [request->identity]]))

(def +gen-report+
  "adds to acceptable media"
  {:produces ["image/png" "image/svg" "image/pdf"]})

(defn ^+gen-report+
  sample-getter
  "Generates report for date"
  [report-id date]
  "here is your report")

(def +new-report+
  "adds to supported media & sets up schema"
  {:schema {:foo :baz}
   :consumes ["application/docx" "image/pdf" "application/odfx"]})

(defn ^+new-report+
  sample-putter
  "Create new report template"
  [body]
  "created")

(defn sample-deleter
  "Delete existing report template.  Report history is not deleted."
  [report-id uid]
  [uid 'deleted report-id])

(defmethod request->identity :mock [& _] {:user "fred",
                                          :uid 21345,
                                          :roles [:admin]})

(def sample-resource
  {:uri ":report-id/:date"               ; used by router AND sets up wrap-binding
   :doc "This doc is for the URI" ; additional docs collected from handlers

   ;; per handler dependencies determine by functions arglists
   ;; below is also used by router
   :get    `sample-getter           ; combine these to generate
   :put    `sample-putter           ; maximum possible supported-methods
   :post   `sample-putter           ; for this URI
   :delete `sample-deleter

   ;; no security information added to auto-docs
   :authentication :mock                ; sets up wrap-authentication
   :disabled-methods [:put]             ; sets up wrap-supported-methods
   :authorization {:anonymous [:get]    ; sets up wrap-authorization
                   :admin [:get :post :delete]}})

(def expected-described-resource
  {:uri ":report-id/:date"
   :doc "This doc is for the URI",

   :get {:handler (resolve `sample-getter)
         :doc "Generates report for date",
         :arglists '([report-id date]),
         :produces ["image/png" "image/svg" "image/pdf"]},

   :delete {:handler (resolve `sample-deleter)
            :doc "Delete existing report template.  Report history is not deleted.",
            :arglists '([request])},

   :post {:handler (resolve `sample-putter)
          :doc "Create new report template",
          :arglists '([body]),
          :schema {:foo :baz},
          :consumes ["application/docx" "image/pdf" "application/odfx"]}

   :disabled-methods [:put],
   :authentication :mock,
   :authorization {:admin [:get :post :delete],
                   :anonymous [:get]}})

(deftest describe-resource-test
  (is (= (describe-resource sample-resource)
         expected-described-resource)))

(def expected-roles {:get    #{:admin
                               :anonymous}
                     :post   #{:admin}
                     :delete #{:admin}})

(deftest description->roles-test
  (is (= (description->roles sample-resource)
         expected-roles)))

(def sm (stack-middleware expected-described-resource :delete {} []))

(def test-req {:uri "deleted-report/today"
               :request-method :delete
               :headers {"Accept" "*/*"}})

(deftest stack-middleware-test
  (prn '(test this tomorrow)))
