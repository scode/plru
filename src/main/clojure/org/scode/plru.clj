(ns
  #^{:author "Peter Schuller <peter.schuller@infidyne.com>",
     :doc "A persistent LRU (Least-Recently-Used) cache."}
  org.scode.plru)

(defn make-lru
  "Create an empty LRU cache with the given maximum size. The maximum
   size determines when entries are evicted from the cache to make
   room for new ones."
  [max-size]
  (assert (> max-size 0))
  { :kvmap {}                ; key/value map of actual items
    :rkmap (sorted-map)      ; recenticity -> key
    :krmap {}                ; key -> recenticity
    :size 0
    :max-size max-size
    :mutation-counter 0 })

(defn- remove-oldest
  "pre-condition: cache is non-empty"
  [cache]
  (let [[recenticity key] (first (:rkmap cache))]
    (conj cache
          [:kvmap (dissoc (:kvmap cache) key)]
          [:rkmap (dissoc (:rkmap cache) recenticity)]
          [:krmap (dissoc (:krmap cache) key)]
          [:size (- (:size cache) 1)])))

(defn lru-put
  "Put the given key/value association in the cache, and return the
resulting cache. If the cache is full, the returned cache will have
evicted the least recently used entry contained in the cache."
  [cache key value]
  (let [had-key (contains? (:kvmap cache) key)
        should-remove (and (not had-key) (>= (:size cache) (:max-size cache)))
        new-kvmap (conj (:kvmap cache) [key value])
        new-size (if had-key (:size cache) (+ 1 (:size cache)))
        old-r (if had-key ((:krmap cache) key) nil)
        new-rkmap (let [with-new-added (conj (:rkmap cache) [(:mutation-counter cache) key])]
                    (if old-r
                      (dissoc with-new-added old-r)
                      with-new-added))
        new-krmap (conj (:krmap cache) [key (:mutation-counter cache)])
        new-mutation-counter (+ 1 (:mutation-counter cache))]
    (let [new-cache (conj cache
                          [:kvmap new-kvmap]
                          [:rkmap new-rkmap]
                          [:krmap new-krmap]
                          [:size new-size]
                          [:mutation-counter new-mutation-counter])]
      (if should-remove
        (remove-oldest new-cache)
        new-cache))))

(defn lru-get
  "Get a value from the cache, if there is one. The optional parameter
not-found is the object that will be returned if there is no entry in
the cache by the given key. If not given, the default is nil. Returns
[value new-cache], where value is the value obtained (or the default),
and new-cache is the new version of the cache updated to reflect the
fact that the key was accessed (i.e., it is now the most recently used
entry in the cache)."
  ([cache key]
     (lru-get cache key nil))
  ([cache key not-found]
     (if (contains? (:kvmap cache) key)
         (let [new-rkmap (conj (dissoc (:rkmap cache) ((:krmap cache) key)) [(:mutation-counter cache) key])
               new-krmap (conj (dissoc (:krmap cache) key) [key (:mutation-counter cache)])
               new-mutation-counter (+ 1 (:mutation-counter cache))]
           [(get (:kvmap cache) key) (conj cache
                        [:rkmap new-rkmap]
                        [:krmap new-krmap]
                        [:mutation-counter new-mutation-counter])])
          [not-found cache])))

(defn lru-peek
  "Like lru-get except it only returns the value obtained, and makes
   no attempt to create a new cache reflecting the access in its
   internal data structure."
  ([cache key]
     (lru-peek cache key nil))
  ([cache key not-found]
     (if (contains? (:kvmap cache) key)
       (get (:kvmap cache) key)
       not-found)))

(defn lru-contains?
  "Returns whether the cache contains the given key."
  [cache key]
  (contains? (:kvmap cache) key))
