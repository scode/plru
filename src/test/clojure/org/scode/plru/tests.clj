(ns #^{:author "Peter Schuller <peter.schuller@infidyne.com>"}
  org.scode.plru.tests
  (:require [org.scode.plru :as plru])
  (:use [clojure.test]))

(deftest basic-creation
  (plru/make-lru 2)) ; 1 not yet supported

(deftest get-non-existing
  (let [[val _] (plru/lru-get (plru/make-lru 2) "key")]
    (is (= nil val) "empty lru should give nil back")))

(deftest peek-non-existing
  (is (= nil (plru/lru-peek (plru/make-lru 2) "key")) "empty lru should peek nil"))

(deftest get-default
  (let [[val _] (plru/lru-get (plru/make-lru 2) "key" "defaultval")]
    (is (= "defaultval" val) "default value should be given on non-existent key")))

(deftest peek-default
  (is (= "default" (plru/lru-peek (plru/make-lru 2) "key" "default"))
      "empty lru should peek default value"))

(deftest get-existing
  (let [c (plru/lru-put (plru/make-lru 10) "key" "value")
        [val _] (plru/lru-get c "key")]
    (is (= "value" val) "should get existing value assocaited with key")))

(deftest peek-existing
  (let [c (plru/lru-put (plru/make-lru 10) "key" "value")]
    (is (= "value" (plru/lru-peek c "key")))))

(deftest eviction-of-unused
  (let [c (plru/make-lru 2)
        c1 (plru/lru-put c "key1" "val1")
        c2 (plru/lru-put c1 "key2" "val2")
        [val _] (plru/lru-get c2 "key1")]
    (is (= "val1" val) "key1 should not yet be evicted")
    (let [c3 (plru/lru-put c2 "key3" "val3")
          [val _] (plru/lru-get c3 "key1")]
      (is (= nil val) "key1 should now be evicted"))))

(deftest nil-values
  (let [c (plru/lru-put (plru/make-lru 2) "key1" nil)]
    (is (= nil (let [[val _] (plru/lru-get c "key1" "default")] val))
        "nil value should be found by get")
    (is (= nil (plru/lru-peek c "key1" "default"))
        "nil value should be found by peek")))

(deftest lru-contains?
  (let [c (plru/lru-put (plru/make-lru 2) "key1" "value")]
    (is (plru/lru-contains? c "key1"))
    (is (not (plru/lru-contains? c "key2")))))

(deftest max-size-1
  (let [c (plru/lru-put (plru/make-lru 1) "key" "value")]
    (is (= "value" (plru/lru-peek c "key")))
    (is (= "value" (let [[val _] (plru/lru-get c "key")] val)))
    (let [c2 (plru/lru-put c "key2" "value2")]
      (is (= "value2" (plru/lru-peek c2 "key2"))))))

(deftest resize
  (let [c (plru/lru-put (plru/lru-put (plru/lru-put (plru/lru-put (plru/make-lru 4) "key1" "value") "key2" "value") "key3" "value") "key4" "value")
        resized (plru/lru-resize c 2)]
    (is (= "value" (plru/lru-peek c "key4")) "lru should basically work first")
    (is (= nil (plru/lru-peek resized "key1")) "key1 should have been eviced")
    (is (= nil (plru/lru-peek resized "key2")) "key2 should have been evicted")))

(run-tests)

