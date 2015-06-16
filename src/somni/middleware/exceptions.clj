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
  "
  Returns a function that takes an r (request or response).  It invokes next-fn
  with r, catches exceptions thrown by next-fn.

  on-error is invoked with r merged with ex-data and ex-details added as :error.
  "
  ([next-fn on-error]

   {:pre [next-fn on-error]}

   (fn [r]
     (let [x (try (next-fn r) (catch Exception e e))]
       (if (instance? Throwable x)
         (on-error (assoc (merge r (ex-data x))
                     :error (ex-details x)))
         x))))

  ([next-fn] (wrap-uncaught-exceptions next-fn server-error)))
