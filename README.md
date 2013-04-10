incstats
========

A clojure library that calculates simple statistics incrementally, i.e. with constant memory costs and results can be queried online.

incstats can be used to calculate:

* Mean (either simple, weighted, or exponentially weighted)
* Variance (biased variance, either simple, weighted, or exponentially weighted)

# Usage

First a stopwatch helper macro for the examples below

    (defmacro stopwatch-nanos
      [body]
      `(let [start# (. System (nanoTime))
             ret# ~body]
         (- (. System (nanoTime)) start#)))


Example: **Calculate the simple mean of a bunch of stopwatch measurements of (Thread/sleep 0)**

    (let [stats (atom nil)] ; 'stats' will store the statistical data
      (dotimes [x 5]
        (let [nanos-elapsed (stopwatch-nanos (Thread/sleep 0))]
          (println "nanos elapsed: " nanos-elapsed)
          ;;
          ;; the following line is where a new data point is added and the stats get updated
          ;;
          (swap! stats update-mean (double nanos-elapsed)))
        ;;
        ;; print the stats
        ;;
        (println "mean:" (:mean @stats) " raw results:" @stats)))

Example: **Calculate the exponentially weighted mean, using a weight factor of 0.8**

    (let [stats (atom nil) ; 'stats' will store the statistical data
          alpha 0.8        ; the weight factor
          update-exponentially-weighted-mean (create-update-exponentially-weighted-variance alpha)]
      (dotimes [x 5]
        (let [nanos-elapsed (stopwatch-nanos (Thread/sleep 0))]
          (println "nanos elapsed: " nanos-elapsed)
          ;;
          ;; the following line is where a new data point is added and the stats get updated
          ;;
          (swap! stats update-exponentially-weighted-mean (double nanos-elapsed)))
        ;;
        ;; print the stats
        ;;
        (println "mean:" (:mean @stats) " raw results:" @stats)))

Example: **Calculate the weighted mean, variance and standard deviation, using some random weights**

    (let [stats (atom nil) ; 'stats' will store the statistical data
          some-random-weights [0.1 2.0 2.0 3.0]]
      (doseq [weight some-random-weights]
        (let [nanos-elapsed (stopwatch-nanos (Thread/sleep 0))]
          (println "nanos elapsed: " nanos-elapsed)
          ;;
          ;; the following line is where a new data point is added and the stats get updated
          ;;
          (swap! stats update-weighted-variance [(double nanos-elapsed) weight]))
        ;;
        ;; print the stats
        ;;
        (println "mean:" (:mean @stats) " std dev:" (Math/sqrt (:variance @stats)) " variance:" (:variance @stats))))

# License

Copyright © 2013 Eugen Dück

Distributed under the Eclipse Public License, the same as Clojure.

# References

This library makes use of the Tony Finch's 2009 paper "Incremental calculation of weighted mean and variance"
