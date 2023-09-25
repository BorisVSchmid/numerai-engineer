(ns engineer.core-submit
  (:require [engineer.submit :as submit]))


(defn -main []

  (submit/submit-live {:stable_dir "merger_results" :filename "v42_TRAIN_meansharpe_OPTIM_top20valimean_mineradiff50" :model_slot :engineer_public_rv1})

  ;; Forced termination of the program.
  ;;
  (shutdown-agents)
  (System/exit 0)
)