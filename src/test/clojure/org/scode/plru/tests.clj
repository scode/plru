(ns #^{:author "Peter Schuller <peter.schuller@infidyne.com>"}
  org.scode.plru.tests
  (:require [org.scode.plru :as plru])
  (:use [clojure.test]))

(deftest basic-creation
  (plru/make-lru 2)) ; 1 not yet supported

(deftest get-non-existing
  (let [[val _] (plru/lru-get (plru/make-lru 2) "key")]
    (is (= nil val) "empty lru should give nil back")))

(deftest peak-non-existing
  (is (= nil (plru/lru-peak (plru/make-lru 2) "key")) "empty lru should peak nil"))

(deftest get-default
  (let [[val _] (plru/lru-get (plru/make-lru 2) "key" "defaultval")]
    (is (= "defaultval" val) "default value should be given on non-existent key")))

(deftest peak-default
  (is (= "default" (plru/lru-peak (plru/make-lru 2) "key" "default"))
      "empty lru should peak default value"))

(deftest get-existing
  (let [c (plru/lru-put (plru/make-lru 10) "key" "value")
        [val _] (plru/lru-get c "key")]
    (is (= "value" val) "should get existing value assocaited with key")))

(deftest peak-existing
  (let [c (plru/lru-put (plru/make-lru 10) "key" "value")]
    (is (= "value" (plru/lru-peak c "key")))))

(deftest eviction-of-unused
  (let [c (plru/make-lru 2)
        c1 (plru/lru-put c "key1" "val1")
        c2 (plru/lru-put c1 "key2" "val2")
        [val _] (plru/lru-get c2 "key1")]
    (is (= "val1" val) "key1 should not yet be evicted")
    (let [c3 (plru/lru-put c2 "key3" "val3")
          [val _] (plru/lru-get c3 "key1")]
      (is (= nil val) "key1 should now be evicted"))))

(run-tests)

