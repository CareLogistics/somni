;;; Copyright (c) Care Logistics, inc. All rights reserved.
;;; The use and distribution terms for this software are covered by the
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.html at the root of this
;;; distribution.
;;; By using this software in any fashion, you are agreeing to be bound by
;;; the terms of this license.
;;; You must not remove this notice, or any other, from this software.

(ns somni.middleware.validation
  (:require [schema.core :as s]
            [somni.http.errors :refer [malformed-request]]))

;; TODO: herbert v. prismatic

(defn wrap-request-validation
  "
  Returns 400 if the body of a post, put or patch does not match the
  schema.
  "
  [handler schema]

  {:pre [handler schema]}

  (fn [{:as request :keys [body request-method]}]
    (if-some [errors (and (#{:post, :put, :patch} request-method)
                          (s/check schema body))]
      (assoc (malformed-request request) :errors errors)
      (handler request))))
