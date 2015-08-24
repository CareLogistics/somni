(ns somni.middleware.etag
  (:import (org.apache.commons.codec.digest DigestUtils)))


(defn- not-modified-response [etag]
  {:status 304 :headers {"Etag" etag}})

(defn etag-generator [resp]
  (DigestUtils/md5Hex (str (:body resp))))

(defn- cached-response
  "Attach an etag header to response header. If old-etag and new-etag match then return a 304."
  [old-etag new-etag response]
  (if (= old-etag new-etag)
    (not-modified-response new-etag)
    (assoc-in response [:headers "Etag"] new-etag)))

(defn wrap-etag
  "Generates an etag header for a response body according to etag-generator
   and transforms response accordingly."
  [handler]
  (fn [request]
    (let [old-etag (get-in request [:headers "if-none-match"])
          response (handler request)
          new-etag (etag-generator response)]
      (cached-response old-etag new-etag response))))