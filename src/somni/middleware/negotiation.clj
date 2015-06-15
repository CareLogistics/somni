(ns somni.middleware.negotiation)

(def request-methods #{:get, :put, :post, :delete})

(defn wrap-supported-methods
  "
  Returns 405 if an unsupported http method is made in the request.
  "
  [handler ops]

  {:pre [handler]}

  (let [ops (set ops)]
    (fn [{:as request :keys [request-method]}]
      (if-not (get ops request-method)
        (unsupported-method request)
        (handler request)))))

(defmulti ->clj :mime-type)
(defmulti clj-> :mime-type)

(def builtin-media-handlers
  [{:supports  #{:octet-stream, :*}
    :mime-type "application/octet-stream"
    :from      identity}

   {:supports  #{:x-www-form-urlencoded}
    :mime-type "application/x-www-form-urlencoded"
    :to        form-decode}

   {:supports  #{:edn :clojure}
    :mime-type "application/edn"
    :from      pr-str
    :to        edn/read-string}])

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
                body (body->str body (:charset mime))]

            (if-some [data (when des (des body))]
              (handler (assoc request :body data))
              (unsupported-media request)))

     :else (handler request))))
