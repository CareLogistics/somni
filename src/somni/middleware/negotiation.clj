(ns somni.middleware.negotiation
  (:require [somni.http.errors :refer [unsupported-method
                                       unsupported-media
                                       not-acceptable
                                       server-error]]
            [somni.http.mime :refer [parse-mime parse-accept]]
            [somni.http.forms :refer [form-decode]]
            [somni.misc :refer [realize-string]]
            [clojure.edn :as edn]))

(defn wrap-supported-methods
  "Returns 405 if an unsupported http method is made in the request."
  [handler ops]

  {:pre [handler]}

  (let [ops (set ops)]
    (fn [{:as request :keys [request-method]}]
      (if-not (get ops request-method)
        (unsupported-method request)
        (handler request)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defn- request->content-type
  [req body]
  (-> (or (:content-type req)
          (get-in req [:headers "Content-Type"])
          "application/octet-stream")
      parse-mime
      :media))

(defmulti ->clj
  "deserialize based upon content-type of request"
  (fn [mime-type body] mime-type))

(defmethod ->clj :edn     [_ body] (edn/read-string body))
(defmethod ->clj :clojure [_ body] (edn/read-string body))
(defmethod ->clj :x-www-form-urlencoded [_ body] (form-decode body))
(defmethod ->clj :octet-stream [_ body] body)

#_
(defn deserialize
  [{:as req :keys body}]
  (let [body (realize-string body )]))

#_ (def builtin-media-handlers
  [

   {:supports  #{:x-www-form-urlencoded}
    :->clj     form-decode}

   {:supports  #{:edn :clojure}
    :clj->     pr-str

    :mime-type "application/edn"
    :->clj     edn/read-string}])

(def clj->clj edn/read-string)


(defn request->accept
  [request]
  (some-> (or (get-in request [:headers "Accept"])
              (get-in request [:headers "accept"]))
          parse-accept))

(defn- media-handler<->mime
  "
  Associates a media handler to a mime type or returns nil.
  "
  [mhs mime]
  (letfn [(mh-lookup [mime-value]
            (first (filter #(get (:supports %) mime-value) mhs)))]

    (when-some [mh (or (mh-lookup (:media  mime))
                       (mh-lookup (:suffix mime)))]

      (merge mime mh))))


(defn- lookup-serializers
  "
  Returns all mime-types with associated media handlers that are acceptable.
  "
  [mhs accept]
  (for [mime (parse-accept accept)
        :let [mime (media-handler<->mime mhs mime)]
        :when (:from mime)]
    mime))






(defn- lookup-deserializer
  "
  Returns a deserializer for the given content type or nil.
  "
  [mhs content-type]
  (let [mh (media-handler<->mime mhs (parse-mime content-type))]
    (when (:to mh) mh)))

(defn- wrap-content-negotiation
  "
  Checks that content negotition can be fulfilled for the request.  If neither
  the accept or content-type are usable, middleware returns status 406 and 415
  respectively.
  "
  [handler media-handlers]

  ^{:mhs media-handlers}
  (fn [{:as request
       :keys [body content-type]
       :or   {content-type "application/octet-stream"}
       {:strs [accept Accept]} :headers}]

    (let [[ser] (lookup-serializers media-handlers (or accept Accept))
          des (lookup-deserializer media-handlers content-type)]

      (cond
       (and body (not des)) (unsupported-media request)

       (nil? ser) (not-acceptable request)

       :else (merge (handler request)
                    ;; Need to attach something here as the serializer
                    ;; won't have access to the original request.
                    (when ser {::mime ser}))))))

(defn- wrap-serialization
  "
  Serializes body of response with best available media handler.
  "
  [next-fn media-handlers]

  ^{:mhs media-handlers}
  (fn [{:as response
       :keys [body ::mime status headers]}]

    (cond
     (nil? body) (next-fn response)     ; Nothing to serialize

     (or (get headers "content-type")   ; TODO: + handlers control Accepts
         (get headers "Content-Type")) (next-fn response)

     (nil? mime) (if (>= status 400)    ; errors skip serialization
                   (next-fn response)
                   (server-error response))

     ;; TODO: do the correct thing with the charset
     :else (let [{:keys [from charset mime-type]} mime
                 data (from body)]

             (next-fn
              (-> response
                  (assoc :body data)
                  (assoc-in [:headers "Content-Type"] mime-type)))))))

(defn- wrap-deserialization
  "
  Deserializes body of request with best available media handler.
  "
  [handler media-handlers]

  ^{:mhs media-handlers}
  (fn [{:as   request
       :keys [content-type body]
       :or   {content-type "application/octet-stream"}}]

    (cond
     body (let [mime (lookup-deserializer media-handlers content-type)
                des  (:to mime)
                body (realize-string body (:charset mime))]

            (if-some [data (when des (des body))]
              (handler (assoc request :body data))
              (unsupported-media request)))

     :else (handler request))))
