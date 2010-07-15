(ns
  #^{:author "Peter Schuller <peter.schuller@infidyne.com>",
     :doc "A persistent LRU (Least-Recently-Used) cache."}
  org.scode.plru)

(defn- guaranteeing
  "Guarantees (by asserting) that (ok? val) evaluates to true, then
   returns val."
  ([ok? val reason]
     (guaranteeing ok? val))
  ([ok? val]
     (assert (ok? val))
     val))

(defn make-lru
  "Create an empty LRU cache with the given maximum size. The maximum
   size determines when entries are evicted from the cache to make
   room for new ones."
  [max-size]
  { :kvmap {}                ; key/value map of actual items
    :rkmap (sorted-map)      ; recenticity -> key
    :krmap {}                ; key -> recenticity
    :size 0
    :max-size (guaranteeing #(> %1 1) max-size "implementation breaks if less than 2"),
    :mutation-counter 0 })

(defn- remove-oldest
  [cache]
  (let [[recenticity key] (first (:rkmap cache))]
    (conj cache
          [:kvmap (dissoc (:kvmap cache) key)]
          [:rkmap (dissoc (:rkmap cache) recenticity)]
          [:krmap (dissoc (:krmap cache) key)]
          [:size (- (:size cache) 1)])))

(defn lru-put
  "Returns a new cache with the given value inserted, associated with
   the given key. If the cache is full, the returned cache will be
   missing the least recently used entry already contained in the
   cache."
  [cache key value]
  (assert (not (= nil value))) ; nil is not supported

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
  "Returns [value new-cache], where new-cache is a new cache with its
   internal statistics updated to reflect the fact the the value
   associated with the given key was accessed (i.e., it is the most
   recently used entry in the cache).

   The optional parameter not-found is the object that will be
   returned if there is no entry in the cache by the given key. If not
   given, the default is nil."
  ([cache key]
     (lru-get cache key nil))
  ([cache key not-found]
     (let [not-found-sym (gensym)
           value (get cache :kvmap not-found-sym)]
       (if (= value not-found-sym)
         not-found
         (let [new-rkmap (conj (dissoc (:rkmap cache) ((:krmap cache) key)) [(:mutation-counter cache) key])
               new-krmap (conj (dissoc (:krmap cache) key) [key (:mutation-counter cache)])
               new-mutation-counter (+ 1 (:mutation-counter cache))]
           [value (conj cache
                        [:rkmap new-rkmap]
                        [:krmap new-krmap]
                        [:mutation-counter new-mutation-counter])])))))

(defn lru-peak
  "Like lru-get except it only returns the value obtained, and makes
   no attempt to create a new cache reflecting the access in its
   internal data structure."
  ([cache key]
     (lru-peak cache key nil))
  ([cache key not-found]
     (let [not-found-sym (gensym)
           value (get cache :kvmap not-found-sym)]
       (if (= value not-found-sym)
         not-found
         value))))
