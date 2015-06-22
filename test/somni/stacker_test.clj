(ns somni.stacker-test
  (:require [somni.stacker :refer :all]
            [clojure.test :refer :all]
            [schema.core :as s]))

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
  [report-id]
  "deleted")

(def sample-resource-def
  {:uris ["w/e"                         ; used by router AND
          ":w/:e"]                      ; sets up wrap-binding

   :doc "This doc is for the URI" ; additional docs collected from handlers

   ;; below is also used by router
   :get    `sample-getter           ; combine these to generate
   :put    `sample-putter           ; maximum possible supported-methods
   :post   `sample-putter           ; for this URI
   :delete `sample-deleter

   ;; per handler dependencies determine by functions arglists
   ;; no security information added to auto-docs
   :security :jwt                       ; sets up wrap-authentication
   :ops [:get :post :put]               ; sets up wrap-supported-methods
   :roles {:anonymous [:get]            ; sets up wrap-authorization
           :admin [:get :post :delete]}})

(def expected-described-resource
  {:uris ["w/e" ":w/:e"]
   :doc "This doc is for the URI",

   :get {:handler (resolve `sample-getter)
         :doc "Generates report for date",
         :arglists '([report-id date]),
         :produces ["image/png" "image/svg" "image/pdf"]},

   :delete {:handler (resolve `sample-deleter)
            :doc "Delete existing report template.  Report history is not deleted.",
            :arglists '([report-id])},

   :put {:handler (resolve `sample-putter)
         :doc "Create new report template",
         :arglists '([body]),
         :schema {:foo :baz},
         :consumes ["application/docx" "image/pdf" "application/odfx"]}

   :post {:handler (resolve `sample-putter)
          :doc "Create new report template",
          :arglists '([body]),
          :schema {:foo :baz},
          :consumes ["application/docx" "image/pdf" "application/odfx"]}

   :security :jwt,
   :ops [:get :post :put],
   :roles {:admin [:get :post :delete], :anonymous [:get]}})
