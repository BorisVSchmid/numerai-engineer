(ns engineer.stats
  {:clj-kondo/config '{:lint-as {uncomplicate.commons.core/with-release clojure.core/let}}}
  (:require [uncomplicate.commons.core :refer [with-release]])
  (:require [uncomplicate.neanderthal.core :refer [transfer! dot nrm2 sum dim subvector scal!]]
    [uncomplicate.fluokitten.core :refer [fmap]]
    [uncomplicate.neanderthal.math :as math]
    [uncomplicate.neanderthal.vect-math :as vmath])
  (:import  [org.apache.commons.math3.stat.ranking NaturalRanking])
  (:import  [org.apache.commons.math3.stat.ranking TiesStrategy]))

(defn mean [numbers]
  (let [sum (reduce + numbers)
        count (count numbers)]
    (/ sum count)))

(defn stddev [numbers]
  (let [mean (mean numbers)
        sum-of-squares (reduce + (map #(Math/pow (- % mean) 2) numbers))
        count (count numbers)]
    (Math/sqrt (/ sum-of-squares count))))

(defn sharpe-ratio
  [returns]
  (/ (mean returns) (stddev returns)))

(defn zscore!
  [V!]
  (let [n (dim V!)
        mu (/ (sum V!) n)]
    (vmath/linear-frac! V! (* -1 mu))
    (let [div (nrm2 V!)]
      (scal! (/ (math/sqrt (dec n)) (if (zero? div) 1.0 div)) V!))))

;; Default NAN strategy is FAILED, which is fine. No way not to check, but it is not as expensive as resolveTIE.
;; Is there a cheaper TiesStrategy?
;;
;; As ranking is so expensive, should I only do ranking when storing a new CR into CR-bucket, and work with cdf-inv-norm until then?
(def NR (NaturalRanking. (. TiesStrategy SEQUENTIAL)))

(defn rank!
  "Takes a neanderthal vector, and returns a ranked neanderthal vector."
  [V!]
  (transfer! (.rank NR (transfer! V! (double-array (dim V!)))) V!))

(defn rank01!
  "Takes a neanderthal vector, and returns a rank ranging from just above 0.01 to around 0.99 for sequences of length 5000.
   Duplicate numer.ai's method. Even though it has a disbalance"
  [V!]
  (scal! (/ (dim V!)) (vmath/linear-frac! (rank! V!) -0.5)))

(defn gaussianize!
  "Transforms a fv rank to just above 0 and just below 1, then into a normal distribution with mean 0, and stddev 1. If things fail, it is likely because (max V!)"
  [V!]
  (vmath/cdf-norm-inv! V!))

(defn pow32ish! [V!]
  (with-release [sign (fmap math/signum V!)]
    (vmath/abs! V!)
    (vmath/pow32! V!)
    (vmath/mul! V! sign)))

(defn pearson-vv
  "Pearson correlation of two vectors in Neanderthal. It is a rearrangement of the classical cov(Vx,vY)/(stdVx*stdVy).
   See https://www.real-statistics.com/descriptive-statistics/measures-variability/measures-variability-detailed/

   transposing along X, or linear scaling along X doesn't affect pearson correlation. 
   It can give values (just?) outside the -1 to 1 range, where it shouldn't. But in general it matches.
   "
  [vX vY]
  (let [sumX (sum vX)
        sumY (sum vY)
        corr (/ (- (* (dim vX) (dot vX vY)) (* sumX sumY))
               (math/sqrt (* (- (* (dim vX) (math/sqr (nrm2 vX))) (math/sqr sumX))
                            (- (* (dim vX) (math/sqr (nrm2 vY))) (math/sqr sumY)))))]
    (float corr)))

(defn transform-target
  "Presumes the target is between 0 and 1"
  [target]
  (scal! 4.0 target)
  (vmath/linear-frac! target -2.0)
  (pow32ish! target))

(defn numerai-corr [targets predictions]
  (->> predictions
    rank01!
    gaussianize!
    pow32ish!
    (pearson-vv targets)))

(defn score-PRED
  [prediction all-truths era-splits]
  (zipmap (keys era-splits)
    (map (fn [[_ era-split]]
           (let [pred (subvector prediction (:index era-split) (:length era-split))]
             (apply merge 
               (for [T (keys all-truths)]
                 (let [truth (subvector (all-truths T) (:index era-split) (:length era-split))]
                   (hash-map T (hash-map :corr (numerai-corr truth pred))))))))
      era-splits)))