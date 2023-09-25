(ns engineer.core-predict
  (:require [engineer.fetch :as fetch])
  (:require [engineer.predict :as predict]))


(defn -main []

  ;;
  ;; Fetch the new live data.
  ;;

  (fetch/fetch-live-data)
  
  ;; Create and evaluate the merging of the different folds.
  ;; Let the filename reflect what you do with it in merge
  ;; 
  ;; Note that merge will take whatever .nippy files are in /results to generate the merger.
  ;;
  (predict/predict-live {:stable_dir "merger_results" :filename "v42_TRAIN_meansharpe_OPTIM_top20valimean_mineradiff50"})

  ;; Forced termination of the program.
  ;;
  (shutdown-agents)
  (System/exit 0)
)