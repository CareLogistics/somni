(ns somni.middleware.auth)

(defn wrap-access-control
  "
  Wraps the handler in an ACL check.  ACLs are specified on http
  methods.  If acl-fn is nil, the user's role will be extracted
  from [:identity :role] of the request.

  Roles are compared by name; :abc, 'abc & \"abc\" are the same role.

  Returns 403 if ACL check fails.
  "
  [handler acls & [acl-fn policy]]

  {:pre [handler]}

  (let [role-lookup (or acl-fn #(get-in % [:identity :role]))
        acls (into {} (for [[k v] acls] [k (set (map name v))]))]

    (cond
     (seq acls) (fn [{:as request :keys [request-method]}]
                  (let [macl (get acls request-method)
                        role (role-lookup request)
                        role (and role (name role))]

                    (if (or (nil? macl)
                            (macl role))
                      (handler request)
                      (access-denied request))))

     (= policy :deny) access-denied
     :else            handler)))

(defn wrap-authentication
  "
  Extremely simple authentication.  If the security function returns
  a value, that value is associated with the request as the identity
  and the handler will be invoked.

  Returns a 401 if sec-fn returns nil.
  "
  [handler sec-fn]

  {:pre [handler sec-fn]}

  (fn [request]
    (if-some [id (sec-fn request)]
      (handler (assoc request :identity id))
      (not-authenticated request))))
