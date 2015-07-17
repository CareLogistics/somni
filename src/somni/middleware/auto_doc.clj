;;; Copyright (c) Care Logistics, inc. All rights reserved.
;;; The use and distribution terms for this software are covered by the
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.html at the root of this
;;; distribution.
;;; By using this software in any fashion, you are agreeing to be bound by
;;; the terms of this license.
;;; You must not remove this notice, or any other, from this software.

(ns somni.middleware.auto-doc)

(defn wrap-options
  "
  Returns 200 with body that describes the resource.
  "
  [handler resource-desc]

  {:pre [handler resource-desc]}

  ;; TODO: make something pretty out of that resource definition
  (fn [{:as request :keys [request-method]}]
    (if (= request-method :options)
      {:status       200
       :content-type "application/text"
       :body         resource-desc}
      (handler request))))
