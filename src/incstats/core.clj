(ns incstats.core)

(defn update-weighted-mean
  "Takes old-stats and a [value weight] tuple and updates the weighted
   mean, or initializes it if old-stats is not given."
  ([]
     (update-weighted-mean [0 0]))
  ([[value weight]]
     {:mean (if (zero? weight) (* value weight) value) :weight-sum weight})
  ([{:keys [mean weight-sum] :as old-stats} [value weight :as data]]
     (if-not old-stats
       (update-weighted-mean data)
       (let [newWeightSum (+ weight-sum weight)]
         {:mean (+ mean (* (/ weight newWeightSum) (- value mean)))
          :weight-sum newWeightSum}))))

(defn update-mean
  "Takes old-stats and a value and updates the simple mean, or
   initializes it if old-stats is not given."
  ([]
     (update-weighted-mean))
  ([value]
     (update-weighted-mean [value 1]))
  ([old-stats value]
     (if-not old-stats
       (update-mean value)
       (update-weighted-mean old-stats [value 1]))))

(defn create-update-exponentially-weighted-mean
  "Takes an alpha (where 0 < alpha <= 1) and returns a function that
   takes old-stats and a value and updates the exponentially weighted
   mean, or initializes it if old-stats is not given."
  [alpha]
  (fn update-exponentially-weighted-mean
    ([]
       (update-exponentially-weighted-mean 0))
    ([value]
       {:mean value})
    ([{:keys [mean] :as old-stats} value]
       (if-not old-stats
         (update-exponentially-weighted-mean value)
         {:mean (+ mean (* alpha (- value mean)))}))))

(defn update-weighted-variance
  "Takes old-stats and a [value weight] tuple and updates the weighted
   mean and variance, or initializes it if old-stats is not given."
  ([]
     (update-weighted-variance [0 0]))
  ([[value weight]]
     (assoc (update-weighted-mean [value weight]) :unnormalized-variance 0 :variance 0))
  ([{:keys [mean weight-sum unnormalized-variance] :as old-stats} [value weight :as data]]
     (if-not old-stats
       (update-weighted-variance data)
       (let [new-mean-stats (update-weighted-mean old-stats data)
             new-mean (:mean new-mean-stats)
             new-weight-sum (:weight-sum new-mean-stats)
             new-unnormalized-variance (+ unnormalized-variance (* weight (- value new-mean) (- value mean)))]
         (assoc new-mean-stats
           :unnormalized-variance new-unnormalized-variance
           :variance (if (zero? new-weight-sum) 0 (/ new-unnormalized-variance new-weight-sum)))))))

(defn update-variance
  "Takes old-stats and a value and updates the simple mean and
   variance, or initializes it if old-stats is not given."
  ([]
     (update-weighted-variance [0 0]))
  ([value]
     (update-weighted-variance [value 1]))
  ([{:keys [mean weight-sum unnormalized-variance] :as old-stats} value]
     (update-weighted-variance old-stats [value 1])))

(defn create-update-exponentially-weighted-variance
  "Takes an alpha (where 0 < alpha <= 1) and returns a function that
   takes old-stats and a value and updates the exponentially weighted
   mean ans variance, or initializes it if old-stats is not given."
  [alpha]
  (fn update-exponentially-weighted-variance
    ([]
       (update-exponentially-weighted-variance 0))
    ([value]
       {:mean value :variance 0})
    ([{:keys [mean variance] :as old-stats} value]
       (if-not old-stats
         (update-exponentially-weighted-variance value)
         (let [diff (- value mean)
               incr (* alpha diff)]
           {:mean (+ mean incr) :variance (* (- 1 alpha) (+ variance (* diff incr)))})))))
