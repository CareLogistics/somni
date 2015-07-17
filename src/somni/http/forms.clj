;;; Copyright (c) Care Logistics, inc. All rights reserved.
;;; The use and distribution terms for this software are covered by the
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.html at the root of this
;;; distribution.
;;; By using this software in any fashion, you are agreeing to be bound by
;;; the terms of this license.
;;; You must not remove this notice, or any other, from this software.

(ns somni.http.forms
  (:require [somni.misc :refer [str->map]]))

(def ^:private form-rexp #"\s*&\s*|\s*=\s*")

(defn form-decode
  "This is basic, no frills x-www-form-urlencoded deserializer to Clojure map,
  Keys as keywords, values as clojure data.
  "
  [body] (str->map body form-rexp))
