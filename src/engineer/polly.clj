(ns engineer.polly
  (:require [clojure.data.json :as json])
  (:require [clojure.pprint :as pprint])
  (:require [clojure.edn :as edn])
  (:require [tick.core :as tick])
  (:require [clj-http.client :as client])
  ;;
  ;; fetch, predict and submit code
  (:require [engineer.fetch :as fetch])
  (:require [engineer.predict :as predict])
  (:require [engineer.submit :as submit]))
  

;;
;;
(def modelname "engineer_public_rv1")
(def tokens (edn/read-string (slurp "secret/upload-key.secret")))
(def modelIDs (edn/read-string (slurp "secret/model-ids.secret")))

;;
;; Fetching Functions
;;
;; https://api-tournament.numer.ai/
;;
(defn current-round []
  (let [rounds (-> (client/get "https://api-tournament.numer.ai" {:query-params {"query" "{rounds(tournament:8){number}}"}})
                 :body
                 (json/read-str :key-fn keyword)
                 :data
                 :rounds)]
    (:number (apply merge-with max rounds))))

(defn round-open? []
  (-> (client/get "https://api-tournament.numer.ai" {:query-params {"query" (str "{roundDetails(roundNumber:" (current-round) ",tournament:8){status}}")}})
    :body
    (json/read-str :key-fn keyword)
    :data
    :roundDetails
    :status
    (not= "closed"))) ;; alternative is "open"?


(defn get-id [modelname] 
  (-> (client/get "https://api-tournament.numer.ai" {:query-params {"query" (str "{v3UserProfile(modelName:\"" modelname \""){id}}")}})
    :body
    (json/read-str :key-fn keyword)
    :data
    :v3UserProfile
    :id))


(defn get-submission-details [modelname]
  (-> (client/get "https://api-tournament.numer.ai"
          {:headers {"Authorization" (str "Token " (:public tokens) "$" (:private tokens))}
           :query-params {"query" (str "{model(modelId:\"" (get-id modelname) \""){latestSubmissions(latestNRounds:1){filename\nroundOpen\nroundClose\nroundNumber\nstatus\ntimestamp}}}")}})
    :body
    (json/read-str :key-fn keyword)
    :data
    :model
    :latestSubmissions
    first))

(defn good-submission? [modelname]
  (let [current-round (current-round)
        submission-details (get-submission-details modelname)
        submitted-round (get-in submission-details [:roundNumber])
        submission (assoc (dissoc (get-submission-details modelname) :roundNumber) :current-round current-round :last-submitted-round submitted-round)]
    (if (and (:filename submission-details) (not= "queued" (:status submission-details)))
      (do
        (println "\nSubmission looks good:")
        (pprint/pprint submission)
        true)
      (do
        (println "\nSubmission looks problematic:")
        (pprint/pprint submission)
        false))))



;; Main loop logic
(defn -main []

  (loop []
    
    (println)
    (println (tick/now))
    
    (try
      
      (when (round-open?)
        
        ;; Check if the valkyries did their thing
        (println "\nCheck if model needs to be run")
        (if (good-submission? modelname)
          
          ;; already submitted?
          (println "Model already submitted for this round. If you want to resubmit use clj -M:predict and clj -M:submit on the command-line.")
          
          (do

            (println "Fetch live data")
            (fetch/fetch-live-data)
            
            (println "Predict model")
            (predict/predict-live {:stable_dir "merger_results" :filename "v42_TRAIN_meansharpe_OPTIM_top20valimean_mineradiff50"})
            
            (println "Submit model")
            (submit/submit-live {:stable_dir "merger_results" :filename "v42_TRAIN_meansharpe_OPTIM_top20valimean_mineradiff50" :model_slot (keyword modelname)})

            (println "Done."))))
      
      (catch Exception e
        (println "Encountered an error:" (.getMessage e))))
    
    ;; Wait 3 minutes
    (println "Wait 3 minutes")
    (Thread/sleep (* 3 60 1000))  
    
    (recur)))


