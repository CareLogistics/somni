(ns somni.stacker-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [somni.http.errors :refer [server-error]]
            [somni.stacker :refer :all]))

(defn ^{:produces ["image/png" "image/svg" "image/pdf"]}
  sample-getter
  "Generates report for date"
  [report-id date]
  (ex-info "Report not found" {:report-id report-id, :date date}))

(defn ^{:consumes ["application/docx" "image/pdf" "application/odfx"],
        :schema {:foo :baz}}
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

(def sample-resource
  {:uri ":report-id/:date"     ; used by router AND sets up wrap-binding
   :doc "This doc is for the URI" ; additional docs collected from handlers

   ;; per handler dependencies determine by functions arglists
   ;; below is also used by router
   :get    #'sample-getter              ; combine these to generate
   :post   #'sample-putter              ; for this URI
   :delete #'sample-deleter})

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
            :doc "Delete existing report template.  Report history is not deleted.",
            :arglists '([user report-id date])}})

(deftest describe-resource-test
  (is (= (#'somni.stacker/describe-resource sample-resource)
         expected-described-resource)))

(deftest stack-middleware-test
  (let [sm (#'somni.stacker/configure-handler
            expected-described-resource :delete {} identity)

        test-req {:uri "wolf-parade/today"
                  :request-method :delete
                  :identity {:user "fred"}}]

    (is (= (:body (sm test-req))
           "[\"delete-report\",\"wolf-parade\",\"today\",[\"user\",\"fred\"]]"))

    (is (= (:body (sm (assoc test-req :uri "matzah")))
           "[\"delete-report-template\",\"matzah\",[\"user\",\"fred\"]]"))))

(deftest error-test
  (let [sm (#'somni.stacker/configure-handler
            expected-described-resource :get {} server-error)

        test-req {:uri "wolf-parade/today"
                  :request-method :get}]

    (is (= (:status (sm test-req))
           500))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bug-16 unit test

(defn bug16-handler
  [catalog-conn code entry codetable]
  [code entry codetable])

(def bug16-resource
  {:uri "codes/:code/:entry"
   :doc "Test case resources for bug-16 fix"
   :put #'bug16-handler})

(deftest bug16-test
  (let [described-rsc (#'somni.stacker/describe-resource bug16-resource)
        sm (#'somni.stacker/configure-handler described-rsc :put {} identity)
        test-req-1 {:uri "/codes/foo/bar"
                    :request-method :put
                    :headers {"content-type" "application/json;charset=UTF-8"
                              "content-length" 54}
                    :body "{\"ordinal\":1,\"name\":\"Foo\",\"value\":\"Bar\"}"}]
    (is (= "[\"foo\",\"bar\",{\"ordinal\":1,\"name\":\"Foo\",\"value\":\"Bar\"}]"
           (:body (sm test-req-1)))
        (with-out-str (clojure.pprint/pprint described-rsc)))))
