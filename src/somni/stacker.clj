;;; Copyright (c) Care Logistics, inc. All rights reserved.
;;; The use and distribution terms for this software are covered by the
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.html at the root of this
;;; distribution.
;;; By using this software in any fashion, you are agreeing to be bound by
;;; the terms of this license.
;;; You must not remove this notice, or any other, from this software.

(ns somni.stacker
  "Configures middleware based upon resource definition."
  (:require [somni.misc :refer :all]
            [somni.middleware.injectors :refer [inject-deps]]
            [somni.middleware.validation :refer [wrap-request-validation]]
            [somni.middleware.extractions :refer [wrap-extractions]]
            [somni.middleware.negotiator :refer [wrap-negotiator]]
            [somni.middleware.bindings :refer [attach-bindings]]
            [somni.middleware.to-ring :refer [wrap-response-as-ring]]
            [somni.middleware.exceptions :refer [wrap-uncaught-exceptions]]))

(def ops #{:get :put :post :delete :any})

(def merged-meta #{:doc :arglists :schema :consumes :produces})

(defn- describe-resource
  [resource]

  {:pre [(map? resource)
         (string? (:uri resource))
         (some resource ops)]}

  (let [handlers
        (for [op ops
              :let [handler (resource op)]
              :when handler]
          [op (assoc
               (into {} (filter (comp merged-meta key) (meta handler)))
               :handler handler)])]

    (reduce (fn [rd [op desc]] (assoc rd op desc))
            (apply dissoc resource ops)
            handlers)))

(defn- config-stacker
  [resource-desc op deps on-error]
  {:handler (get-in resource-desc [op :handler])
   :uri     (get-in resource-desc [:uri])
   :op       op
   :extract (#{:any :get} op)
   :schema  (get-in resource-desc [op :schema])
   :conneg  (not-any? (get-in resource-desc [op]) [:produces :consumes])
   :on-error on-error
   :deps     deps})

(defn- stack-middleware
  [{:keys [handler deps uri extract schema conneg on-error auth acls]}]

  {:pre [handler uri]}

  (cond-> handler
    :always      (inject-deps deps)
    :always      (attach-bindings uri)
    extract      (wrap-extractions uri)
    :always      (wrap-response-as-ring)
    (seq schema) (wrap-request-validation schema)
    :always      (wrap-uncaught-exceptions on-error) ; serializable errors
    conneg       (wrap-negotiator)
    :always      (wrap-uncaught-exceptions on-error))) ; unserializable errors

(def ^:private configure-handler (comp stack-middleware config-stacker))

(defn stack
  [resource deps on-error]

  (let [resource-desc (describe-resource resource)]
    (for [op ops :when (op resource-desc)]
      [op
       (:uri resource-desc)
       (configure-handler resource-desc op deps on-error)])))
