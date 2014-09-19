;;; Copyright (c) Care Logistics, inc. All rights reserved.
;;; The use and distribution terms for this software are covered by the
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;; which can be found in the file epl-v10.html at the root of this
;;; distribution.
;;; By using this software in any fashion, you are agreeing to be bound by
;;; the terms of this license.
;;; You must not remove this notice, or any other, from this software.

(ns carelogistics.perf-tests
  (:require [carelogistics.somni :as somni]
            [clojure.test :refer :all]
            [compojure.core :refer (GET routes)]
            [ring.mock.request :refer (request)]
            [criterium.core :as criterium]))

(def uris ["/index.html" "/a.html" "/b.html" "/c.html" "/d.html" "/e.html"
           "/blog/f.html" "/blog/g.html" "/blog/h.html" "/blog/i.html"
           "/gallery/j.html" "/gallery/k.html" "/gallery/l.html"
           "/sites/m.html" "/sites/n.html" "/sites/o.html" "/sites/p.html"
           "/m/q.html" "/m/r.html" "/m/s.html" "/m/t.html" "/m/u.html"
           "/foo/bar/v.html" "/foo/baz/w.html" "/foo/quux/x.html"
           "/lambda/y.html" "/lambda/z.html" "/lambda/a.html"])

(def reqs (vec (map (partial request :get) uris)))

(defn e [_] {:status 200 :body "e"})

(def compojure-1
  (let [ctx (routes (GET "/index.html" [] e))
        req (request :get "/index.html")]
    (fn [] (ctx req))))

(def compojure-7
  (let [ctx (routes
             (GET "/index.html" [] e)
             (GET "/a.html" [] e)
             (GET "/b.html" [] e)
             (GET "/c.html" [] e)
             (GET "/d.html" [] e)
             (GET "/e.html" [] e)
             (GET "/blog/f.html" [] e))
        reqs (vec (take 7 reqs))]
    (fn [] (ctx (rand-nth reqs)))))

(def compojure-14
  (let [ctx (routes
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
             (GET "/sites/m.html" [] e))
        reqs (vec (take 14 reqs))]
    (fn [] (ctx (rand-nth reqs)))))

(def c-28-routes (routes
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

(def compojure-28 (fn [] (c-28-routes (rand-nth reqs))))

(def compojure-28-worst
  (let [req (last reqs)] (fn [] (c-28-routes req))))

(def somni-1
  (let [h (somni/make-handler
           [{:uris (take 1 uris) :handler :x}]
           {:x e}
           {})
        req (request :get "/index.html")]
    (fn [] (h req))))

(def somni-28
  (let [h (somni/make-handler
           [{:uris uris :handler :x}]
           {:x e}
           {})]
    (fn [] (h (rand-nth reqs)))))

(def somni-10k
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
    (fn [] (h (rand-nth reqs)))))

(deftest somni-setup-test []
  (is (= (:body (somni-1))
         (:body (somni-28))
         (:body (somni-10k))
         "e")))

(deftest compojure-setup-test []
  (is (= (compojure-1)
         (compojure-7)
         (compojure-14)
         (compojure-28)
         {:status 200, :headers {}, :body "e"})))

(defn benchmark-compojure []
  (println "#### Compojure with 1 route ####")
  (criterium/bench (compojure-1))
  (println)

  (println "#### Compojure with 7 routes ###")
  (criterium/bench (compojure-7))
  (println)

  (println "#### Compojure with 14 routes ##")
  (criterium/bench (compojure-14))
  (println)

  (println "#### Compojure with 28 routes ##")
  (criterium/bench (compojure-28))
  (println)

  (println "#### Compojure worst case routing")
  (criterium/bench (compojure-28-worst))
  (println))

(defn benchmark-somni []
  (println "#### Somni with 1 route ####")
  (criterium/bench (somni-1))
  (println)

  (println "#### Somni with 28 routes ##")
  (criterium/bench (somni-28))
  (println)

  (println "#### Somni with 10k routes #")
  (criterium/bench (somni-10k))
  (println))

(defn run-benchmarks []
  (benchmark-compojure)
  (println)
  (println "----------------------------")
  (println)
  (benchmark-somni))
