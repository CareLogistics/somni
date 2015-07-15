(ns somni.stacker-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [somni.http.errors :refer [server-error]]
            [somni.middleware.access-control :refer [request->identity]]
            [somni.stacker :refer :all]))

(def +gen-report+
  "adds to acceptable media"
  {:produces ["image/png" "image/svg" "image/pdf"]})

(defn ^+gen-report+
  sample-getter
  "Generates report for date"
  [report-id date]
  (ex-info "Report not found" {:report-id report-id, :date date}))

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
  [user report-id date]
  (if date
    [:delete-report report-id date [:user user]]
    [:delete-report-template report-id [:user user]]))

(defmethod request->identity :mock [& _] {:user "fred",
                                          :uid 21345,
                                          :roles [:admin]})

(def sample-resource
  {:uri ":report-id/:date"               ; used by router AND sets up wrap-binding
   :doc "This doc is for the URI" ; additional docs collected from handlers

   ;; per handler dependencies determine by functions arglists
   ;; below is also used by router
   :get    #'sample-getter           ; combine these to generate
   :put    #'sample-putter           ; maximum possible supported-methods
   :post   #'sample-putter           ; for this URI
   :delete #'sample-deleter

   ;; no security information added to auto-docs
   :authentication :mock                ; sets up wrap-authentication
   :disabled-methods [:put]             ; sets up wrap-supported-methods
   :authorization {:get #{:admin :anonymous}    ; sets up wrap-authorization
                   :post #{:admin}
                   :delete #{:admin}}})

(def expected-described-resource
  {:uri ":report-id/:date"
   :doc "This doc is for the URI"

   :post {:handler #'somni.stacker-test/sample-putter
          :doc "Create new report template"
          :arglists '([body])
          :schema {:foo :baz}
          :consumes ["application/docx" "image/pdf" "application/odfx"]}

   :get {:handler #'somni.stacker-test/sample-getter
         :doc "Generates report for date"
         :arglists '([report-id date])
         :produces ["image/png" "image/svg" "image/pdf"]}

   :delete {:handler #'somni.stacker-test/sample-deleter
            :doc "Delete existing report template.  Report history is not deleted."
            :arglists '([user report-id date])}

   :authentication :mock
   :disabled-methods [:put]
   :authorization {:get #{:admin :anonymous}
                   :post #{:admin}
                   :delete #{:admin}}})

(deftest describe-resource-test
  (is (= (#'somni.stacker/describe-resource sample-resource)
         expected-described-resource)))

(deftest stack-middleware-test
  (let [sm (#'somni.stacker/configure-handler
            expected-described-resource :delete {} [] identity)

        test-req {:uri "wolf-parade/today"
                  :request-method :delete}]

    (is (= (:body (sm test-req))
           "[\"delete-report\",\"wolf-parade\",\"today\",[\"user\",\"fred\"]]"))

    (is (= (:body (sm (assoc test-req :uri "matzah")))
           "[\"delete-report-template\",\"matzah\",[\"user\",\"fred\"]]"))))

(deftest error-test
  (let [sm (#'somni.stacker/configure-handler
            expected-described-resource :get {} [] server-error)

        test-req {:uri "wolf-parade/today"
                  :request-method :get}]

    (is (= (:status (sm test-req))
           500))))
