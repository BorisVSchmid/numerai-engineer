(ns engineer.core-fetch
  (:require [engineer.fetch :as fetch]))

(defn -main []
  ;;
  ;; Fetch the new training data.
  ;;

  (fetch/fetch-new-training-data)

  ;; Only need to re-calculate the correlation table between the features once,
  ;; or when the number of features changes (but that would mean editing the 
  ;; fetch/get-latest-urls function as well)

  ; (fetch/calculate-new-correlation-table)

  ;; Forced termination of the program.
  ;;
  (shutdown-agents)
  (System/exit 0)
)  