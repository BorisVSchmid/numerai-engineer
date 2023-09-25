(ns engineer.core-train
  (:require [engineer.hillclimber :as hill]))

(defn -main []

  ;; train a few folds. We have 1080 eras, and a buffer of 15 eras + 170 validation eras + 15 buffer eras.
  ;; so the last fold is at 880.
  ;;
  ;; Eras 1-400 were used to generate the rain features, so there is some leakage there.
  ;;
  (hill/train-fold {:firstbufferera "0001" :numiters 5000})
  (hill/train-fold {:firstbufferera "0201" :numiters 5000})
  (hill/train-fold {:firstbufferera "0401" :numiters 5000})
  (hill/train-fold {:firstbufferera "0601" :numiters 5000})
  (hill/train-fold {:firstbufferera "0801" :numiters 5000})
  ;(hill/train-fold {:firstbufferera "0880" :numiters 5000})

  ;; You might want to continue training a fold. If so, you have to specify the bufferera, the old numiters, the new numiters, and the random seed.
  ;; With that information, hillclimber can find and continue with the file, reading in the chunks to recreate the history pool.
  ;;
  ;;(hill/continue-fold {:firstbufferera "0875" :oldnumiters 5000 :numiters 10000 :rseed 3661})

  ;; Forced termination of the program.
  ;;
  (shutdown-agents)
  (System/exit 0)
)  