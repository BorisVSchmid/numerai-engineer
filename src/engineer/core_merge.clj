(ns engineer.core-merge
  (:require [engineer.merger :as merge]))


(defn -main []

  ;; Create and evaluate the merging of the different folds.
  ;; Let the filename reflect what you do with it in merge
  ;; 
  ;; Note that merge will take whatever .nippy files are in /results to generate the merger. It is smart enough
  ;; to not process the shorter and longer numiters of the same model run as two folds.
  ;;
  (merge/merge-folds {:stable_dir "merger_results" :filename "v42_TRAIN_meansharpe_OPTIM_top20valimean_mineradiff50" :ensemblesize 20 :mineradiff 50})

  ;; Forced termination of the program.
  ;;
  (shutdown-agents)
  (System/exit 0)
)