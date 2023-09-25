(ns engineer.predict
  {:clj-kondo/config '{:lint-as {uncomplicate.commons.core/with-release clojure.core/let}}}
  (:require [clojure.walk :refer [postwalk]])
  (:require [clojure.java.io :as io])
  (:require [clojure.edn :as edn])
  (:require [clojure.string :as string])
  (:require [clojure.data.csv :as csv])
  (:require [taoensso.nippy :refer [thaw-from-file]]
            [neanderthal-stick.nippy-ext])
  (:require [uncomplicate.neanderthal.core :refer [imin imax amax dim scal! zero sum nrm2 axpy!]]
            [uncomplicate.neanderthal.real :as real]
            [uncomplicate.neanderthal.math :as math]
            [uncomplicate.neanderthal.vect-math :as vmath]
            [uncomplicate.commons.core :refer [with-release]]))

;;
;; Constants
;;
(defonce data_dir "data")
(declare constant-zero)
(declare constant-zero-template)
(declare ids)


;;
;; Functions
;;
(defn adjust-path [coll]
  (postwalk #(if (string? %) (string/replace % "/training/" "/live/") %) coll))

(defn zscore!
  [V!]
  (let [n (dim V!)
        mu (/ (sum V!) n)]
    (vmath/linear-frac! V! (* -1 mu))
    (let [div (nrm2 V!)]
      (scal! (/ (math/sqrt (dec n)) (if (zero? div) 1.0 div)) V!))))

(defn normalize!
  "Transforms a range to just above 0 and just below 1"
  [V!]
  (scal! (/ (amax V!)) V!)
  (let [mi (- (real/entry V! (imin V!)) 0.001)
        MA (+ (real/entry V! (imax V!)) 0.001)]
    ;; bring to close to 0-1
    (vmath/linear-frac! V! (* -1 mi))
    (scal! (/ (- MA mi)) V!)))

(defn infer-uber
  "Takes an frequency table of transcripts, and evaluate them.
   These are now the neanderthal predictions."
  [stable_dir filename]
  ;; Sum the transcripts together, multiply each unique one by frequency.
  (let [transcript-freqs (thaw-from-file (str stable_dir "/" filename ".nippy"))
        outcome (zero constant-zero-template)]
    (doseq [[T F] (into [] transcript-freqs)]
        ;;
        (with-release [feature ((eval (adjust-path T)))]
          (axpy! F (zscore! feature) outcome)))
    (normalize! outcome)
    outcome))

(defn predict-live [options]

  ;(def options {:stable_dir "merger_results" :filename "v41_TRAIN_meansharpe_OPTIM_top20valimean_mineradiff50"})
  
  ;; These two files only exist after fetch-live has been run.
  (def constant-zero (list (symbol "taoensso.nippy" "thaw-from-file") (str data_dir "/live/feature_zero_point_zero.nippy")))
  (def constant-zero-template (eval constant-zero))
  (def ids (edn/read-string (slurp (str data_dir "/live-ids.edn"))))
  
  ;; predictions.
  (doall (with-open [writer (io/writer (str (:stable_dir options) "/" (:filename options) ".csv"))]
        (csv/write-csv writer [["id" "prediction"]])
        (csv/write-csv writer (mapv #(vector %1 %2) ids (seq (infer-uber (:stable_dir options) (:filename options))))))))