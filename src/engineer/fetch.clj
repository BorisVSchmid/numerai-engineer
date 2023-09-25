(ns engineer.fetch
  {:clj-kondo/config '{:lint-as {uncomplicate.commons.core/with-release clojure.core/let}}}
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:require [tech.v3.dataset :as ds]
            [tech.v3.libs.parquet])
  (:require [taoensso.nippy :refer [freeze-to-file thaw-from-file]]
            [neanderthal-stick.nippy-ext])
  (:require [com.climate.claypoole :as cp])
  (:require [clj-http.client :as client])
  (:require [engineer.stats :refer [pearson-vv]])
  (:require [uncomplicate.neanderthal.native :refer [fv]]
            [uncomplicate.commons.core :refer [with-release]]))


;; ## Declares and constants
;;
(def number-of-cores 8)


;; ## Fetching Functions
;;
;; https://api-tournament.numer.ai/
;; query{listDatasets}
;; query{dataset(filename:"v3/numerai_live_data_int8.parquet")}
;;
(defn get-latest-urls []
  (let [training (client/get "https://api-tournament.numer.ai" {:query-params {"query" "{dataset(filename:\"v4.2/train_int8.parquet\")}"} :accept :json})
        validation (client/get "https://api-tournament.numer.ai" {:query-params {"query" "{dataset(filename:\"v4.2/validation_int8.parquet\")}"} :accept :json})
        live (client/get "https://api-tournament.numer.ai" {:query-params {"query" "{dataset(filename:\"v4.2/live_int8.parquet\")}"} :accept :json})]
    (hash-map
      :training (get-in (json/read-str (:body training) :key-fn keyword) [:data :dataset])
      :validation (get-in (json/read-str (:body validation) :key-fn keyword) [:data :dataset])
      :live (get-in (json/read-str (:body live) :key-fn keyword) [:data :dataset]))))

(defn fetch!
  "makes an HTTP request and fetches the binary object"
  [url]
  (let [req (client/get url {:as :byte-array :throw-exceptions false})]
    (when (= (:status req) 200)
      (:body req))))

(defn fetch-parquet!
  "downloads and stores the tournament on disk. ChatGPT's suggestion how to deal with data > 2GB."
  [url filename]
  (let [input-stream (java.io.BufferedInputStream. (.openStream (java.net.URL. url)))
        output-file (java.io.File. (str "data/" filename))
        output-stream (java.io.FileOutputStream. output-file)]
    (try
      (io/copy input-stream output-stream)
      (finally
        (.close input-stream)
        (.close output-stream)))))


;; ## Other functions
;; 
;; * Sanitizing column names to not contain apostrophes etc (just a-zA-Z0-9_)
;; * Create a zero feature
;; * Store nippy training files
;;
(defn sanitize-columns [name]
  (reduce str (re-seq #"[a-zA-Z0-9_]+" name)))

(defn translate ^Float [n]
  (case n
    2 (float 0.0) ;; center or missing data.
    3 (float 0.68)
    1 (float -0.68)
    4 (float 1.64)
    0 (float -1.64)
    0.5 (float 0.0))) ;; We don't translate the targets, so the 0.5's we encounter are features-only.

(defn store-training [DATA]

  ;; order is training eras, followed by validation eras
  (doseq [F (filter #(.contains % "feature") (ds/column-names DATA))]
    (println (sanitize-columns F))
    (freeze-to-file (str "data/training/" (sanitize-columns F) ".nippy")
      (fv (map translate (ds/column DATA F)))))

  ;; feature_zero_point_zero
  (println "feature_zero_point_zero")
  (freeze-to-file (str "data/training/" "feature_zero_point_zero" ".nippy")
    (fv (repeat (count (ds/column DATA "era")) 0)))

  ;; Save the truths in the same way.
  (doseq [T (filter #(.contains % "target") (ds/column-names DATA))]
    (println (sanitize-columns T))
    (freeze-to-file (str "data/training/" (sanitize-columns T) ".nippy")
      (fv (ds/column DATA T)))))

(defn store-live [LIVE]
  ;;
  ;; order is training eras, followed by validation eras
  (doseq [F (filter #(.contains % "feature") (ds/column-names LIVE))]
    ;;(println (sanitize-columns F))
    (freeze-to-file (str "data/live/" (sanitize-columns F) ".nippy")
      (fv (map translate (ds/column LIVE F)))))
  ;;
  ;; feature_zero_point_zero
  ;;(println "feature_zero_point_zero")
  (freeze-to-file (str "data/live/" "feature_zero_point_zero" ".nippy")
    (fv (repeat (count (ds/column LIVE (first (ds/column-names LIVE)))) 0)))
  
  (spit "data/live-ids.edn" (vec (ds/column LIVE "id"))))

(defn store-correlation-table []
  (let [features (->> (clojure.java.io/file "data/training/")
                   (file-seq)
                   (filter #(.isFile %))
                   (filter #(string/includes? % "feature_"))
                   (map #(.getName %)))
        combinations (partition 1000 1000 [] (set (for [F1 features F2 features] (sort (list F1 F2)))))
        counter (atom 1)
        bucket (atom [])]
    (println (count (reduce into [] combinations)))
    (cp/pdoseq number-of-cores [C combinations]
      (swap! counter inc)
      (println "iter " @counter " of " (count combinations))
      (swap! bucket conj 
        (apply merge (map #(with-release [F1 (thaw-from-file (str "data/training/" (first %)))
                                          F2 (thaw-from-file (str "data/training/" (second %)))]
                             (hash-map % (pearson-vv F1 F2)))
                       C))))
    (freeze-to-file (str "data/v42correlations.nippy") (apply merge @bucket))))


;; some features all have value 2 on int8 for up to era 400.
;; by default, no more missing in 4.2. But doesn't hurt to have.

(defn fetch-new-training-data []

  ;; Fetch
  (fetch-parquet! (:training (get-latest-urls)) "train_int8.parquet")
  (fetch-parquet! (:validation (get-latest-urls)) "validation_int8.parquet")
  
  ;; Read
  ;; We don't translate the targets, so the 0.5's in the targets are where they should be, and the 0.5's in the features
  ;; get translated to 0's (center). Targets with only 0.5's (60D targets at the most recent eras) get caught in the 
  ;; pearson correlations, so all works out.
  (with-release [DATA (ds/concat-inplace
                        (ds/replace-missing (ds/->dataset "data/train_int8.parquet") identity :value 0.5)
                        (ds/replace-missing (ds/->dataset "data/validation_int8.parquet") identity :value 0.5))
                 DATA-ERAS (ds/column DATA "era")]
      
      ;; Store training.
      (store-training DATA)
      
      ;; Last thing: era sizes and indices
      (let [RAW (map #(list (first %) (count %)) (partition-by identity (seq DATA-ERAS)))
          era-order (mapv first RAW)
          era-sizes (mapv second RAW)]
        (spit "data/era-order.edn" era-order)
        (spit "data/era-sizes.edn" (zipmap era-order (map #(hash-map :index (- %1 %2) :length %2) (reductions + era-sizes) era-sizes))))))



(defn calculate-new-correlation-table []
  ;;  
  ;; store the correlations between features (expensive)
  (store-correlation-table))



(defn fetch-live-data []
  ;;
  ;; ## General notes:
  ;;
  ;; All missing gets replaced with a -99. -99 has some advantages above -1 in calculating masks later on.

  ;; Fetch
  (fetch-parquet! (:live (get-latest-urls)) "live_int8.parquet")
  
  ;; Read
  (with-release [LIVE (ds/replace-missing (ds/->dataset "data/live_int8.parquet") identity :value 0.5)]

      ;; Store live.
      (store-live LIVE)))
