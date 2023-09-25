(ns engineer.submit
    (:require [clojure.edn :as edn])
    (:require [clojure.data.json :as json])
    (:require [clj-http.client :as client]))

;;
;; Sensitive data
;;
(def tokens (edn/read-string (slurp "secret/upload-key.secret")))
(def modelIDs (edn/read-string (slurp "secret/model-ids.secret")))

;;
;; Functions
;;
(defn get-submit-details [filename model]
  (let [request (client/get "https://api-tournament.numer.ai"
                            {:headers {"Authorization" (str "Token " (:public tokens) "$" (:private tokens))}
                             :query-params {"query" (str "{submissionUploadAuth(filename:\"" filename "\", modelId:\"" (model modelIDs) "\", tournament:8) {url, filename}}")}
                             :throw-entire-message? true
                             :debug false
                             :accept :json})]
    (get-in (json/read-str (:body request) :key-fn keyword) [:data :submissionUploadAuth])))

(defn submit-file [url file]
      (client/request {:method :put :url url :content-type "text/plain" :body (slurp file)}))

(defn confirm-submission [filename model]
  (let [request (client/post "https://api-tournament.numer.ai"
               {:headers {"Authorization" (str "Token " (:public tokens) "$" (:private tokens))}
                :query-params {"query" (str "mutation{createSubmission (filename:\"" filename "\", modelId:\"" (model modelIDs) "\", tournament:8, version:2) {id}}")}
                :throw-entire-message? true
                :debug false
                :accept :json})]
    (get-in (json/read-str (:body request) :key-fn keyword) [:data :createSubmission])))

(defn all-in-one [stable_dir filename model]
  (Thread/sleep 2000)
  (let [details (get-submit-details filename model)]
      (submit-file (:url details) (str stable_dir "/" filename))
      (confirm-submission (:filename details) model)))


;; Actual submission
(defn submit-live [options]

  ;; v1
  (all-in-one (:stable_dir options) (str (:filename options) ".csv") (:model_slot options))
  (Thread/sleep 5000))


