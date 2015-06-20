(ns somni.negotiator
  (:require [somni.http.errors :refer :all]
            [somni.marshalling.serializers :refer :all]
            [somni.marshalling.deserializers :refer :all]))

(defn wrap-supported-methods
  "Returns 405 if an unsupported http method is made in the request."
  [handler ops]

  {:pre [handler]}

  (let [ops (set ops)]
    (fn [{:as request :keys [request-method]}]
      (if-not (get ops request-method)
        (unsupported-method request)
        (handler request)))))

(defn wrap-content-negotiation
  [handler])




































#_
(defn- wrap-content-negotiation
  "
  Checks that content negotition can be fulfilled for the request.  If neither
  the accept or content-type are usable, middleWare Returns status 406 and 415
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

#_
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

#_
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
