(ns dailyprogrammer.heap)

(declare make-heap bubble-up bubble-down swap parent-index child-indeces verify)

(defn make-max-heap [compare-fn]
  (make-heap compare-fn []))

(defn make-min-heap [compare-fn]
  (make-heap #(compare-fn %2 %1) []))

(defn heap-empty? [heap]
  (empty? (:data heap)))

(defn heap-top [heap]
  (first (:data heap)))

(defn heap-add
  [heap item]
  (let [comp   (:comp heap)
        data   (:data heap)
        len    (count data)
        result (make-heap comp
                          (bubble-up comp (conj data item) len))]
    #_(verify comp (:data result))
    result))

(defn heap-remove [heap]
  (let [comp   (:comp heap)
        data   (:data heap)
        result (make-heap comp
                          (let [len  (count data)
                                data (->> (swap data 0 (dec len))
                                          (take (dec len))
                                          (vec))]
                            (bubble-down comp data 0)))]
    #_(verify comp (:data result))
    result))

(defn heap-seq [heap]
  (if (heap-empty? heap)
    ()
    (let [r        (heap-top heap)
          new-heap (heap-remove heap)]
      (cons r
            (lazy-seq (heap-seq new-heap))))))

(defn- make-heap [comp data]
  {:comp comp
   :data data})

(defn- bubble-up [comp data indx]
  (let [pindx  (parent-index indx)]
    (if (< indx 1)
      data
      (let [parent (nth data pindx)
            child  (nth data indx)]
        (if (pos? (comp child parent))
          (recur comp (swap data indx pindx) pindx)
          data)))))

(defn- bubble-down [comp data indx]
  (let [len           (count data)
        child-indeces (filter #(< % len) (child-indeces indx))
        child-count   (count child-indeces)
        maxc-indx     (condp = child-count
                        2 (let [[i1 i2]   child-indeces
                                [c1 c2]   (map #(nth data %) child-indeces)]
                            (if (pos? (comp c1 c2)) i1 i2))
                        1 (first child-indeces)
                        0 nil)]
    (if maxc-indx
      (let [parent (nth data indx)
            child  (nth data maxc-indx)]
        (if (pos? (comp child parent))
          (recur comp (swap data maxc-indx indx) maxc-indx)
          data))
      data)))

(defn- swap [data i1 i2]
  (assoc data
    i1 (nth data i2)
    i2 (nth data i1)))

(defn- parent-index [indx]
  (quot (dec indx) 2))

(defn- child-indeces [indx]
  (let [base (* indx 2)]
    [(+ base 1) (+ base 2)]))

(defn- verify [comp data]
  (let [indx (-> data count dec)]
    (loop [indx indx]
      (if (>= indx 1)
        (let [pindx (parent-index indx)
              p     (nth data pindx)
              c     (nth data indx)]
          (if-not (pos? (comp p c))
            (do (println data)
                (throw (RuntimeException. (str "Heap property unsatisfied at: " indx))))
            (recur (dec indx))))))))
          
