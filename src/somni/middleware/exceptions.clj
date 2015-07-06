(ns somni.middleware.exceptions
  (:require [somni.http.errors :refer [server-error]]))

(defn ex-details
  "Recursively converts a Java Throwable to clojure map with :exception,
  :message, :stackTrace & :cause"
  [^Throwable e]

  (let [details {:exception  (type e)
                 :message    (.getMessage e)
                 :stackTrace (remove #(re-matches #"^(clojure|java).*" %)
                                     (map str (.getStackTrace e)))}]

    (if-some [cause (.getCause e)]
      (assoc details :cause (ex-details cause))
      details)))

(defn wrap-uncaught-exceptions
  "..."
  ([next-fn on-error]

   {:pre [next-fn on-error]}

   (fn [req]
     (let [resp (try (next-fn req) (catch Exception e e))]
       (if (instance? Throwable resp)
         (on-error (merge {:body {:request req
                                  :details (ex-details resp)}}
                          (ex-data resp)))
         resp))))

  ([next-fn] (wrap-uncaught-exceptions next-fn server-error)))
