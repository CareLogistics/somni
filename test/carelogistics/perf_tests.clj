(ns carelogistics.perf-tests
  (:require [carelogistics.somni :as somni]
            [clojure.test :refer :all]
            [compojure.core :refer (GET routes)]
            [ring.mock.request :refer (request)]))

;;;
(def ^:dynamic *cnt* 500000)

(def uris ["/index.html" "/a.html" "/b.html" "/c.html" "/d.html" "/e.html"
           "/blog/f.html" "/blog/g.html" "/blog/h.html" "/blog/i.html"
           "/gallery/j.html" "/gallery/k.html" "/gallery/l.html"
           "/sites/m.html" "/sites/n.html" "/sites/o.html" "/sites/p.html"
           "/m/q.html" "/m/r.html" "/m/s.html" "/m/t.html" "/m/u.html"
           "/foo/bar/v.html" "/foo/baz/w.html" "/foo/quux/x.html"
           "/lambda/y.html" "/lambda/z.html" "/lambda/a.html"])

(def reqs (vec (map (partial request :get) uris)))

(defn e [req] {:status 200 :body "e"})

(def compojure-7 (routes
                  (GET "/index.html" [] e)
                  (GET "/a.html" [] e)
                  (GET "/b.html" [] e)
                  (GET "/c.html" [] e)
                  (GET "/d.html" [] e)
                  (GET "/e.html" [] e)
                  (GET "/blog/f.html" [] e)))

(deftest compojure-7-test []
  (let [ctx compojure-7
        reqs (vec (take 7 reqs))]

    (is (= (ctx (rand-nth reqs)) {:status 200, :headers {}, :body "e"}))
    (println (format "Time for %d matches using compojure with 7 routes" *cnt*))
    (time (dotimes [_ *cnt*] (ctx (rand-nth reqs))))))

(def compojure-14 (routes
                   (GET "/index.html" [] e)
                   (GET "/a.html" [] e)
                   (GET "/b.html" [] e)
                   (GET "/c.html" [] e)
                   (GET "/d.html" [] e)
                   (GET "/e.html" [] e)
                   (GET "/blog/f.html" [] e)
                   (GET "/blog/g.html" [] e)
                   (GET "/blog/h.html" [] e)
                   (GET "/blog/i.html" [] e)
                   (GET "/gallery/j.html" [] e)
                   (GET "/gallery/k.html" [] e)
                   (GET "/gallery/l.html" [] e)
                   (GET "/sites/m.html" [] e)))

(deftest compojure-14-test []
  (let [ctx compojure-14
        reqs (vec (take 14 reqs))]

    (is (= (ctx (rand-nth reqs)) {:status 200, :headers {}, :body "e"}))
    (println (format "Time for %d matches using compojure with 14 routes" *cnt*))
    (time (dotimes [_ *cnt*] (ctx (rand-nth reqs))))))

(def compojure-28 (routes
                   (GET "/index.html" [] e)
                   (GET "/a.html" [] e)
                   (GET "/b.html" [] e)
                   (GET "/c.html" [] e)
                   (GET "/d.html" [] e)
                   (GET "/e.html" [] e)
                   (GET "/blog/f.html" [] e)
                   (GET "/blog/g.html" [] e)
                   (GET "/blog/h.html" [] e)
                   (GET "/blog/i.html" [] e)
                   (GET "/gallery/j.html" [] e)
                   (GET "/gallery/k.html" [] e)
                   (GET "/gallery/l.html" [] e)
                   (GET "/sites/m.html" [] e)
                   (GET "/sites/n.html" [] e)
                   (GET "/sites/o.html" [] e)
                   (GET "/sites/p.html" [] e)
                   (GET "/m/q.html" [] e)
                   (GET "/m/r.html" [] e)
                   (GET "/m/s.html" [] e)
                   (GET "/m/t.html" [] e)
                   (GET "/m/u.html" [] e)
                   (GET "/foo/bar/v.html" [] e)
                   (GET "/foo/baz/w.html" [] e)
                   (GET "/foo/quux/x.html" [] e)
                   (GET "/lambda/y.html" [] e)
                   (GET "/lambda/z.html" [] e)
                   (GET "/lambda/a.html" [] e)))

(deftest compojure-control-test []
  (let [ctx compojure-28]

    (is (= (ctx (rand-nth reqs)) {:status 200, :headers {}, :body "e"}))
    (println (format "Time for %d matches using compojure with 28 routes" *cnt*))
    (time (dotimes [_ *cnt*] (ctx (rand-nth reqs))))))

(deftest compojure-one-route-test []
  (let [ctx (routes (GET "/index.html" [] e))
        req (request :get "/index.html")]

    (is (= (ctx req) {:status 200, :headers {}, :body "e"}))
    (println (format "Time for %d matches using compojure with one route" *cnt*))
    (time (dotimes [_ *cnt*] (ctx req)))))

(deftest compojure-last-of-28-test []
  (let [ctx compojure-28
        req (request :get "/lambda/a.html")]

    (is (= (ctx req) {:status 200, :headers {}, :body "e"}))
    (println (format "Time for %d matches last route using compojure with 28 routes" *cnt*))
    (time (dotimes [_ *cnt*] (ctx req)))))

(deftest somni-test []
  (let [h (somni/make-handler
           [{:uris uris :handler :x}]
           {:x e}
           {})]

    (is (= (:body (h (rand-nth reqs))) "e"))
    (println (format "Time for %d matches using somni with 28 routes" *cnt*))
    (time (dotimes [_ *cnt*] (h (rand-nth reqs))))))

(deftest somni-one-route-test []
  (let [h (somni/make-handler
           [{:uris (take 1 uris) :handler :x}]
           {:x e}
           {})
        req (request :get "/index.html")]

    (is (= (:body (h req)) "e"))
    (println (format "Time for %d matches using somni with one route" *cnt*))
    (time (dotimes [_ *cnt*] (h req)))))

(deftest somni-big-test []
  (let [paths ["foo" "bar" "baz" "quux" "blog" "assets" "images"]

        rand-path (fn [] (clojure.string/join "/"
                                             (map (fn [_] (rand-nth paths))
                                                  (range (inc (rand-int 4))))))

        rand-name (fn [] (re-find #"\w*" (str (java.util.UUID/randomUUID))))

        uris  (map (fn [_] (str (rand-path) "/" (rand-name))) (range 10000))

        reqs (vec (map (partial request :get) uris))

        h (somni/make-handler
           [{:uris uris :handler :x}]
           {:x e}
           {})]

    h

    (is (= (:body (h (rand-nth reqs))) "e"))
    (println (format "Time for %d matches to 10000 somni routes" *cnt*))
    (time (dotimes [_ *cnt*] (h (rand-nth reqs))))))
