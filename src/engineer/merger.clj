(ns engineer.merger
  {:clj-kondo/config '{:lint-as {uncomplicate.commons.core/with-release clojure.core/let}}}
  (:require [clojure.walk :refer [postwalk]])
  (:require [clojure.java.io :as io])
  (:require [clojure.edn :as edn])
  (:require [clojure.string :as string])
  (:require [taoensso.nippy :refer [freeze-to-file thaw-from-file]])
  (:require [uncomplicate.neanderthal.core :refer [zero axpy!]]
    [uncomplicate.commons.core :refer [with-release]])
  (:require [com.climate.claypoole :as cp])
  (:require [engineer.buildCR :refer [constant-zero]])
  (:require [engineer.hillclimber :refer [minmerge-scores extract-target deep-merge]])
  (:require [engineer.stats :refer [zscore! mean sharpe-ratio transform-target score-PRED]])
  ;; Viz
  (:require [oz.core :as oz]))


;; ## Constants
;;
(defonce data_dir "data")
(declare stable_dir)
(defonce all-eras (edn/read-string (slurp (str data_dir "/era-order.edn"))))
(defonce era-sizes (edn/read-string (slurp (str data_dir "/era-sizes.edn"))))
(defonce constant-zero-template (eval constant-zero)) ;; needed

;;
;;
;; Truths. If we are only optimizing on cyrus here, we can also limit the truths to cyrus
;;
(def cyrus-truth
  "The target that corr is scored on. Missing truth values have 0 as value, and the target ranges from -2.82 to 2.82"
  (let [truths (map #(thaw-from-file (str data_dir "/training/" %))
                 (filter #(and (.contains % "target_cyrus_v4_20") (.contains % "nippy"))
                   (map str (.list (io/file (str data_dir "/training/"))))))
        buffer (first truths)]
    ;;
    ;; transform target to what is used by numerai_corr.
    {"cyrus-truth" (transform-target buffer)}))

(def merged-truth
  "The combined target score your predictions try to match. Missing truth values have 0 as value, and the target ranges from -2.82 to 2.82"
  (let [truths (map #(thaw-from-file (str data_dir "/training/" %))
                 (filter #(and (or
                                 (.contains % "target_ralph")
                                 (.contains % "target_tyler")
                                 (.contains % "target_victor")                               
                                 (.contains % "target_waldo")                               
                                 (.contains % "target_jerome")  ;; jerome added in here based on random comment from MikeP                             
                                 (.contains % "target_cyrus")
                                 (.contains % "target_caroline")
                                 (.contains % "target_sam")
                                 (.contains % "target_xerxes"))
                            (.contains % "nippy")) (map str (.list (io/file (str data_dir "/training/"))))))
        buffer (zero (first truths))]
    (doseq [R truths] (axpy! (/ (count truths)) R buffer))
    ;;
    ;; transform target to what is used by numerai_corr.
    (transform-target buffer)))

(def all-truths (merge cyrus-truth {"merged-truth" merged-truth}))


;; ## Store and Retrieve functions
;;
;; ## Files and selection/buffer/validation era definitions
;;
(def offsets-map {"cyrus-truth" {:corr 0.0} "merged-truth" {:corr -0.015}})

(defn fitness
  [scores validation-eras]
  (let [val-scores (extract-target "cyrus-truth" (select-keys scores validation-eras))
        rawscores-corr (minmerge-scores val-scores :corr {})]
    (mean (remove NaN? rawscores-corr))))


(declare folds)
(declare files)

  

(defn subset
  "Collects samplesize number of top critters, at least 50 iters apart."
  [CR-bucket ensemblesize mineradiff]
  (let [validation-eras (:validation-eras (first CR-bucket))
        data (map #(assoc % :iter (:iter %) :score (fitness (:infer-scores %) validation-eras)) CR-bucket)]
      (loop [d (rest (reverse (sort-by :score data))) bucket [(first (reverse (sort-by :score data)))]]
                (if (and (seq d) (< (count bucket) ensemblesize))
                  (let [next (first d)
                        diff (reduce min (map #(abs (- (:iter %) (:iter next))) bucket))]
                    (recur (rest d) (if (< diff mineradiff) bucket (conj bucket next))))
                  bucket))))
    

(defn display-subsets [all-folds ensemblesize mineradiff]
  (let [folds (reduce into [] (map (fn [F nF] (map #(assoc (select-keys % [:iter :score]) :fold nF :selected false) (subset F (count F) 0))) all-folds (range)))
        reduced-folds (reduce into [] (map (fn [F nF] (map #(assoc (select-keys % [:iter :score]) :fold nF :selected true) (subset F ensemblesize mineradiff))) all-folds (range)))
        spec {:data {:values (concat folds reduced-folds)}
               :facet {:row {:field :fold}}               
               :spec {:width 1500
                      :height 250
                      :layer [{:encoding {:x {:field :iter :type :quantitative}
                                   :y {:field :score :type :quantitative :scale {:zero false}}
                                   :color {:field :name :type :nominal}}
                               :mark {:type "line" :tooltip true}}
                               {:transform [{:filter {:field :selected :equal "true"}}]
                                :encoding {:x {:field :iter :type :quantitative}
                                           :y {:field :score :type :quantitative :scale {:zero false}}}
                                :mark {:type "point" :fill "red" :size 50 :tooltip true}}]
                      }}]
    (oz/view! spec)
    spec))


(defn infer-uber
  "Takes an frequency table of transcripts, and evaluate them.
   These are now the neanderthal predictions."
  [transcript-freqs]
  ;; Sum the transcripts together, multiply each unique one by frequency.
  (let [outcome (zero constant-zero-template)]
    (println (str "processing " (count transcript-freqs) " transcripts."))
    (println "This will take a while when evaluating on the full training dataset.")
    (doseq [[T F] (into [] transcript-freqs)]
        ;;
        (with-release [feature ((eval T))]
          (axpy! F (zscore! feature) outcome)))
      outcome))


;;
;; How does our trained-on merge look like?
;;
(defn calc-scores [predictions]
  (with-release [era-blocksize (inc (int (/ (count all-eras) 2)))
                 scores (doall (apply deep-merge (cp/pmap 2
                                                   #(score-PRED predictions 
                                                      all-truths
                                                      (zipmap % (map era-sizes %)))
                                                   (partition era-blocksize era-blocksize nil all-eras))))]
    (postwalk identity scores)))


(defn report [scores mean-score sharpe-score]
  (let [data-V1 (mapcat (fn [[k v]] (for [st (keys all-truths) tt (list :corr)]
                                      (hash-map :era (Integer/parseInt k)
                                        :score (get-in v [st tt])
                                        :target st
                                        :subtarget tt)))
                  scores)
        V1 {:width 2210 :height 200 :title (format "NUMERAI CORR scores per selection target. Mean: %.3f, Sharpe: %.3f" mean-score sharpe-score)
            :data {:values (filter #(= (:subtarget %) :corr) data-V1)}
            :encoding {:row {:field :target}
                       :x {:field :era :type "ordinal"}
                       :y {:field :score :type "quantitative"}}
            :mark {:type "bar" :width 2.6 :opacity 0.98 :clip true :tooltip {:content "data"}}}]
    (oz/view! V1)
    V1))

(defn merge-folds [options]
  
  ;(def options {:stable_dir "merger_results" :filename "v41_TRAIN_meansharpe_OPTIM_top20valimean_mineradiff50" :ensemblesize 20 :mineradiff 50})  
  ;;
  
  ;; Select the ensemble.
  (let [files (->> (file-seq (io/file "training_results/."))
                  (filter #(.isFile %))
                  (map #(.getName %))
                  (filter #(.contains % "nippy"))
                  (group-by #(first (string/split % #"-numiters")))
                  vals)
        find-highest-iter (fn [c] (reduce max (map #(parse-long (second (string/split % #"-numiters|-chunk"))) c)))
        selected-files (map (fn [c] (let [topiter (find-highest-iter c)]
                                      (filter #(= topiter (parse-long (second (string/split % #"-numiters|-chunk")))) c)))
                                               files)
        all-folds (map (fn [filenames] (reduce into [] (mapv #(thaw-from-file (str "training_results/" %)) filenames))) selected-files)
        vega-spec (display-subsets all-folds (:ensemblesize options) (:mineradiff options))]
    
    ;; Write out visualization of the merger.
    (do (oz/export! [:div {:style {:display "flex" :flex-direction "column"}}
                    [:vega-lite vega-spec]]
          (str (:stable_dir options)  "/" (:filename options) ".html")) nil)
  
    (let [reduced-folds (frequencies (reduce into [] (map :transcript (reduce into [] (map #(subset % (:ensemblesize options) (:mineradiff options)) all-folds)))))
          uber-outcome (infer-uber reduced-folds)
          scores (calc-scores uber-outcome)
          mean-score (->> (extract-target "cyrus-truth" scores)
                       vals
                       (mapcat vals)
                       (map :corr)
                       (remove #(NaN? %))
                       mean)
          sharpe-score (->> (extract-target "cyrus-truth" scores)
                         vals
                         (mapcat vals)
                         (map :corr)
                         (remove #(NaN? %))
                         sharpe-ratio)]
      
    ;; Export the reduced-fold we made  
    (freeze-to-file (str (:stable_dir options) "/" (:filename options) ".nippy") reduced-folds)
      
    ;; Export the score (for what its worth)
    (do (oz/export! [:div {:style {:display "flex" :flex-direction "column"}}
                    [:vega-lite (report scores mean-score sharpe-score)]]
        (str (:stable_dir options)  "/" (:filename options) ".html")) nil)
    )))




  


