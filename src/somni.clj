;;; Copyright (c) Care Logistics, inc. All rights reserved.
;;; The use and distribution terms for this software are covered by the
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.html at the root of this
;;; distribution.
;;; By using this software in any fashion, you are agreeing to be bound by
;;; the terms of this license.
;;; You must not remove this notice, or any other, from this software.

(ns ^{:author "Andrew Garman"}
  somni
  (:require [schema.core :as s]
            [somni.http.errors :refer [server-error
                                       not-found]]
            [somni.middleware.exceptions :refer [wrap-uncaught-exceptions]]
            [somni.router :as router]
            [somni.stacker :as stacker]))

(def somni-schema [stacker/resource-schema])

(defn add-prefix
  [resources prefix]
  (if prefix
    (map #(update-in % [:uri] (partial str prefix "/")) resources)
    resources))

(defn build
  [resources deps &
   {:keys [user-middlewares
           on-missing
           on-error
           uri-prefix]
    :or {on-error   server-error
         on-missing not-found}}]

  (s/validate somni-schema resources)

  (let [resources       (add-prefix resources uri-prefix)
        stacked-resources (mapcat stacker/build-resources resources)
        router          (router/add-routes {} stacked-resources)
        handler         (router/router->handler router)]

    (with-meta
      (-> handler
          (wrap-uncaught-exceptions on-error))
      {:resources resources
       :stacked-resources stacked-resources
       :router router})))
