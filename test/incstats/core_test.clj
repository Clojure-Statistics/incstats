(ns incstats.core-test
  (:use clojure.test
        incstats.core))

(defn- non-incremental-weighted-mean
  [data]
  (let [[multiplied-value-sum weight-sum]
        (reduce (fn [[acc-v acc-w] [v w]] [(+ acc-v (* w v)) (+ acc-w w)]) [0 0] data)]
    (if (zero? weight-sum)
      multiplied-value-sum
      (/ multiplied-value-sum weight-sum))))

(defn- non-incremental-mean
  [data]
  (non-incremental-weighted-mean (map #(vector % 1) data)))

(defn- non-incremental-exponentially-weighted-mean
  [alpha data]
  (let [f (first data)
        r (rest data)]
    (if-not r
      (or f 0)
      (reduce (fn [acc v] (+ (* (- 1 alpha) acc) (* alpha v))) f r))))

(definline square
  [x]
  `(* ~x ~x))

(defn- non-incremental-weighted-variance
  [data]
  (let [mean (non-incremental-weighted-mean data)
        weight-sum (apply + (map second data))]
    (if (zero? weight-sum)
      weight-sum
      (/ (reduce (fn [acc [v w]] (+ acc (* w (square (- v mean))))) 0 data) weight-sum))))

(defn- non-incremental-variance
  [data]
  (non-incremental-weighted-variance (map #(vector % 1) data)))

(defn- non-incremental-exponentially-weighted-variance
  [alpha data]
  (if (not (rest data))
    0
    (let [mean (non-incremental-exponentially-weighted-mean alpha data)]
      (- (non-incremental-exponentially-weighted-mean alpha (map square data)) (square mean)))))

(defn- epsilon=
  [x y]
  (let [epsilon (cond (= Double (class x)) 0.000000000000003 true 0)]
    (if (zero? epsilon)
      (= x y)
      (< (Math/abs (- x y)) epsilon))))

(deftest test-stuff
  (let [data [[1.1 0] [2.1 1] [3.1 2] [4.1 3] [5.1 2]]
        data-no-weight (map first data)]
    (testing "simple-mean"
      (let [expected-values (map #(non-incremental-mean (take (inc %) data-no-weight)) (range (count data-no-weight)))
            actual-values (map :mean (rest (reductions update-mean nil data-no-weight)))]
        (is (= (count expected-values) (count actual-values)))
        (doseq [[expected actual] (map vector expected-values actual-values)]
          (is (epsilon= expected actual)))))
    (testing "weighted-mean"
      (let [expected-values (map #(non-incremental-weighted-mean (take (inc %) data)) (range (count data)))
            actual-values (map :mean (rest (reductions update-weighted-mean nil data)))]
        (is (= (count expected-values) (count actual-values)))
        (doseq [[expected actual] (map vector expected-values actual-values)]
          (is (epsilon= expected actual)))))
    (testing "exponentially-weighted-mean"
      (doseq [alpha [0.1 0.5 1]]
        (let [expected-values (map #(non-incremental-exponentially-weighted-mean alpha (take (inc %) data-no-weight)) (range (count data-no-weight)))
              actual-values (map :mean (rest (reductions (create-update-exponentially-weighted-mean alpha) nil data-no-weight)))]
          (is (= (count expected-values) (count actual-values)))
          (doseq [[expected actual] (map vector expected-values actual-values)]
            (is (epsilon= expected actual))))))
    (testing "simple-variance"
      (let [expected-values (map #(non-incremental-variance (take (inc %) data-no-weight)) (range (count data-no-weight)))
            actual-values (map :variance (rest (reductions update-variance nil data-no-weight)))]
        (is (= (count expected-values) (count actual-values)))
        (doseq [[expected actual] (map vector expected-values actual-values)]
          (is (epsilon= expected actual)))))
    (testing "weighted-variance"
      (let [expected-values (map #(non-incremental-weighted-variance (take (inc %) data)) (range (count data)))
            actual-values (map :variance (rest (reductions update-weighted-variance nil data)))]
        (is (= (count expected-values) (count actual-values)))
        (doseq [[expected actual] (map vector expected-values actual-values)]
          (is (epsilon= expected actual)))))
    (testing "exponentially-weighted-variance"
      (doseq [alpha [0.1 0.5 1]]
        (let [expected-values (map #(non-incremental-exponentially-weighted-variance alpha (take (inc %) data-no-weight)) (range (count data-no-weight)))
              actual-values (map :variance (rest (reductions (create-update-exponentially-weighted-variance alpha) nil data-no-weight)))]
          (is (= (count expected-values) (count actual-values)))
          (doseq [[expected actual] (map vector expected-values actual-values)]
            (is (epsilon= expected actual))))))))

(comment ; the following is just for playing around and testing code for documentation

(defmacro stopwatch-nanos
  [body]
  `(let [start# (. System (nanoTime))
         ret# ~body]
     (- (. System (nanoTime)) start#)))

(deftest test-incrementally
  (testing "actually incremental"
    (stopwatch-nanos (Thread/sleep 0)) ; warmup, as first Thread/sleep is always extremly slow
    (let [stats (atom nil)]
      (dotimes [x 5]
        (let [nanos-elapsed (stopwatch-nanos (Thread/sleep 0))]
          (println "nanos elapsed: " nanos-elapsed)
          (swap! stats update-mean (double nanos-elapsed))))
      (println "mean:" (:mean @stats) " raw results:" @stats))
    (let [stats (atom nil)
          alpha 0.8
          update-exponentially-weighted-mean (create-update-exponentially-weighted-variance alpha)]
      (dotimes [x 5]
        (let [nanos-elapsed (stopwatch-nanos (Thread/sleep 0))]
          (println "nanos elapsed: " nanos-elapsed)
          (swap! stats update-exponentially-weighted-mean (double nanos-elapsed))))
      (println "mean:" (:mean @stats) " raw results:" @stats))
    (let [stats (atom nil)
          some-random-weights [0.1 2.0 2.0 3.0]]
      (doseq [weight some-random-weights]
        (let [nanos-elapsed (stopwatch-nanos (Thread/sleep 0))]
          (println "nanos elapsed: " nanos-elapsed)
          (swap! stats update-weighted-variance [(double nanos-elapsed) weight])))
      (println "mean:" (:mean @stats) " std dev:" (Math/sqrt (:variance @stats)) " variance:" (:variance @stats) " raw results:" @stats))))

)