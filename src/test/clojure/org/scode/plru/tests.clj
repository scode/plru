(ns #^{:author "Peter Schuller <peter.schuller@infidyne.com>"}
  org.scode.plru.tests
  (:require [org.scode.plru :as plru])
  (:use [clojure.test]))

(deftest basic-creation
  (plru/make-lru 2)) ; 1 not yet supported

(deftest get-non-existing
  (let [[val _] (plru/lru-get (plru/make-lru 2) "key")]
    (is val nil)))

(deftest get-default
  (let [[val _] (plru/lru-get (plru/make-lru 2) "key" "defaultval")]
    (is val "defaultval")))

(deftest get-existing
  (let [c (plru/lru-put (plru/make-lru 10) "key" "value")
        [val _] (plru/lru-get c "key")]
    (is val "value")))

(deftest eviction-of-unused
  (let [c (plru/make-lru 2)
        c1 (plru/lru-put c "key1" "val1")
        c2 (plru/lru-put c1 "key2" "val2")
        [val _] (plru/lru-get c2 "key1")]
    (is val "val1")
    (let [c3 (plru/lru-put c2 "key3" "val3")
          [val _] (plru/lru-get c3 "key1")]
      (is val nil))))

(run-tests)

