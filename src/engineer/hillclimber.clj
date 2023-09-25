(ns engineer.hillclimber
  {:clj-kondo/config '{:lint-as {uncomplicate.commons.core/with-release clojure.core/let}}}
  ;; haven't figured out yet how to tell condo about the (cp/pdoseq cpu-threads [a b]) pattern
  (:require [clojure.walk :refer [postwalk]])
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as set])
  (:require [clojure.edn :as edn])
  (:require [clojure.inspector])
  (:require [com.climate.claypoole :as cp])
  ;; 
  ;; Neanderthal and related
  (:require [taoensso.nippy :refer [freeze-to-file thaw-from-file]])
  (:require [neanderthal-stick.nippy-ext :refer [with-real-factory]]) ;; Need this, even when clj-kondo thinks I don't.
  (:require [uncomplicate.neanderthal.core :refer [zero axpy! copy]]
    [uncomplicate.commons.core :refer [with-release release]])
  ;;
  (:require [engineer.buildCR :refer [build-CR constant-zero default-params gen-transcript]]
    [engineer.stats :refer [transform-target zscore! mean sharpe-ratio score-PRED]]
    [engineer.mutateCR])
  ;; 
  ;; Visualization
  (:require [oz.core :as oz]))



;; ## CPU utilization parameters
;; It is better to avoid any threading on the MKL level for these small tasks, so set MKL_NUM_THREADS to 1 in your environment.
;; 
;; With a mutant-pool of 8 individuals (i.e. exploring 8 directions of the gradient), using a cpu-threads of 4 or 8 is useful.
;; Evaluating the performance across 1000 eras gets a modest speedup by doing it in 2 threads as well.
;;
(def cpu-threads 8) 
(def cpu-threads-train 2) ;; 3 and 4 gets to higher speed, but not much.
;; with 8 threads and 2 train-threads, I have about 25% utilization (of 18 physical / 36 logical cores), which translates
;; to an average full use of (* 36 0.25) = 9 / 16 = 56% utilization
;; with 8 threads and 1 train-threads, I have about 15% utilization (of 18 physical / 36 logical cores), which translates
;; to an average full use of (* 36 0.15) = 5.4 / 8 physical cores = 67.5% utilization
;; with 1 thread and 1 train-thread, I have 2.4-2.9% utilization (of 18/36 cores), 
;; 1 thread full utilization would be about 2.7-2.8%. (/ 2.6 2.77). So I am at 93% utilization there.

;; So the main way I lose time is if one of the 8 cpu threads is finished earlier than one of the other.
;; Hard to avoid. Don't really want to sample more than 8 directions of the gradient - no guarantee that
;; helps.




;; ## Declares
;;
(declare continue?)
(declare selection-targets)
(declare validation-eras)
(declare buffer-eras)
(declare selection-eras)
(declare offsets-map)



;; ## Constants and GLOBAL ATOMS
;;
(defonce data_dir "data")
(defonce all-eras (edn/read-string (slurp (str data_dir "/era-order.edn"))))
(defonce era-sizes (edn/read-string (slurp (str data_dir "/era-sizes.edn"))))
(defonce constant-zero-template (eval constant-zero))
;;
(defonce CODEBLOCKS (atom {}))
(defonce TIMER (atom (. System (nanoTime))))
(defonce CR (atom nil))
(defonce CR-bucket (atom []))
(defonce ITER (atom 0))
(defonce MUTANT-POOL (atom []))



;; ## Truths.
;; These are the two targets that we train against - one is the current (june 2023) target on which you are scored in 
;; the numerai tournament (cyrus), and one is a merger of recent targets that help fit cyrus-truth more smoothly.
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
                                 (.contains % "target_alpha")
                                 (.contains % "target_bravo")
                                 (.contains % "target_charlie")                               
                                 (.contains % "target_delta")                               
                                 (.contains % "target_echo")
                                 (.contains % "target_cyrus")
                                 (.contains % "target_jerome")
                                 (.contains % "target_nomi")
                                 (.contains % "target_jeremy")
                                 (.contains % "target_taylor"))
                            (.contains % "nippy")) (map str (.list (io/file (str data_dir "/training/"))))))
        buffer (zero (first truths))]
    (doseq [R truths] (axpy! (/ (count truths)) R buffer))
    ;;
    ;; transform target to what is used by numerai_corr.
    (transform-target buffer)))

(def all-truths (merge cyrus-truth {"merged-truth" merged-truth}))



;; ## Inference function.
;;
(defn infer-CR
  "Takes a CR, adds the transcript codeblock columns together. These are now the predictions."
  [CR]
  ;; Sum the transcripts together. We do this vector by vector
  (let [outcome (zero constant-zero-template)]
    (doseq [T (:transcript CR)]
      ;;
      ;; Check if there is any uncached transcripts. Add them to the pool and feature-neutralize them
      (when-not (contains? @CODEBLOCKS T)
        (with-release [feature ((eval T))]
          (swap! CODEBLOCKS assoc T (zscore! (copy feature))))) ;; added a z-score here, so each feature more-or-less has the same shape.
      ;;
      ;; Stack the transcripts.
      (axpy! 1.0 (@CODEBLOCKS T) outcome))
    outcome))


;; ## Mutating and Training functions
;;
(defn select-mutation
  "Pick a mutation from the default parameters that doesn't reduce the number of transcripts below 10"
  [CR]
  (loop [mutation (rand-nth (:mutate-ratios default-params))]
    (if (and (< (count (:transcript CR)) 10) (#{engineer.mutateCR/delete-block engineer.mutateCR/delete} mutation))
      (do
        (println "not deleting in a tiny genome:" (count (:transcript CR)) mutation)
        (recur (rand-nth (:mutate-ratios default-params))))
      mutation)))

(defn do-mutate
  "Apply mutations to the genome of the PARENT until we find a mutation that results in a different 
   transcript (or we hit 10 recursions). If you apply the mutations recursively to the CHILD, you can
   get interesting behavior as well (and possibly make larger jumps through the search space) 
   but you also have to tweak how often you select for deletion-mutations to avoid stacking up too many 
   transcripts."
  [PARENT]
  (loop [mutation (select-mutation PARENT) recursions 0]
    (let [NEW ((eval mutation) PARENT)
          CHILD (assoc NEW :transcript (gen-transcript (:genome NEW)))]
      (if (and (< recursions 10) (= (:transcript PARENT) (:transcript CHILD)))
        (recur (select-mutation PARENT) (inc recursions))
        ;; 
        ;; We have a mutant
        CHILD))))


;; ## Fitness functions
;;
(defn minmerge-scores
  "This function scores eras for a particular type of scoring metric (currently only :corr), on their minimal
   performance across targets, while allowing for each target to have an offset. The offset map helps adjust
   targets that differ in difficulty.
   
   NaN scores within the collection of scores for a particular era are deleted, and eras which are left
   without scores at all get 0.0 as a score."
  [coll type offsets-map]
  (let [targets (keys (first (vals coll)))]
    (map (fn [era]
           ;; collect the scores for all targets from an era, and add the offset if not NaN.
           (let [era-scores (->>
                              (map (fn [T] (let [raw (get-in coll [era T type])] 
                                             (if (NaN? raw) nil (+ raw (get-in offsets-map [T type] 0.0))))) targets)
                              (remove nil?))]
             ;; calculate a merged score for this list of scores
             (if (empty? era-scores) 0.0 (reduce min era-scores))))
      (keys coll))))

(defn fitness 
  "Fitness optimizes the mean times sharpe of the era scores. Why sharpe, and not sortino or squared distance from some target level?
   That's because high peaks in score might not be harmless or beneficial but a sign of sensitivity that you don't want.
   "
  [scores label offsets-map]
  (let [scores-merged (minmerge-scores scores :corr offsets-map)
        scores-mean (mean scores-merged)
        scores-sharpe (sharpe-ratio scores-merged)
        scores-burn (reduce + (filter neg? scores-merged))
        scores-fitness (* scores-mean scores-sharpe (if (and (neg? scores-mean) (neg? scores-sharpe)) -1 1))]
    (hash-map
      (keyword (str label "_fitness")) scores-fitness
      (keyword (str label "_mean")) scores-mean
      (keyword (str label "_burn")) scores-burn
      (keyword (str label "_sharpe")) scores-sharpe)))

  



;; ## Training functions
;;
(defn deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))

(defn do-train [CHILD]
  (with-release [predictions (infer-CR CHILD)
                 era-blocksize (inc (int (/ (count all-eras) cpu-threads-train)))
                 scores (doall (apply deep-merge (cp/pmap cpu-threads-train 
                                                   #(score-PRED predictions 
                                                      (select-keys all-truths selection-targets)
                                                      (zipmap % (map era-sizes %)))
                                                   (partition era-blocksize era-blocksize nil all-eras))))
                 fitness-map (merge (#'fitness (select-keys scores selection-eras) "selection" offsets-map)
                               (#'fitness (select-keys scores validation-eras) "validation" offsets-map))]
    (postwalk identity (merge (assoc CHILD :infer-scores scores) fitness-map))))



;; ## Generate initial critter
;;
(defn generate-adam
  []
  (reset! CR (build-CR default-params))
  (swap! CR assoc :train-eras selection-eras)
  (swap! CR do-train))

;; ## Reporting
;;
(defn extract-target [target scores]
  (zipmap (keys scores) (map #(select-keys % [target]) (vals scores))))

(defn V1-report []
  (let [data-V1 (concat (filter #(>= (:iter %) 200) (map #(let [scores (extract-target "cyrus-truth" (:infer-scores %))]
                                                            (assoc (merge 
                                                                     (#'fitness (select-keys scores selection-eras) "selection" {})
                                                                     (#'fitness (select-keys scores validation-eras) "validation" {}))
                                                              :grouping "cyrus-truth"
                                                              :validation_fitness (:validation_fitness (#'fitness (select-keys scores validation-eras) "validation" offsets-map))
                                                              :transcript_count (count (:transcript %))
                                                              :iter (:iter %)))
                                                      @CR-bucket))
                  (filter #(>= (:iter %) 200) (map #(let [scores (extract-target "merged-truth" (:infer-scores %))]
                                                      (assoc (#'fitness (select-keys scores selection-eras) "selection" offsets-map)
                                                        :grouping "merged-truth"
                                                        :transcript_count (count (:transcript %))
                                                        :iter (:iter %)))
                                                @CR-bucket))
                  (filter #(>= (:iter %) 200) (map #(assoc (#'fitness (select-keys (:infer-scores %) selection-eras) "selection" offsets-map)
                                                      :grouping "all-truths"
                                                      :transcript_count (count (:transcript %))
                                                      :iter (:iter %))
                                                @CR-bucket)))
        V1 {:facet {:column {:field :subtype}}
            :data {:values (mapcat (fn [e] (map #(hash-map :type (case %1
                                                                   :transcript_count :selection
                                                                   :selection_fitness :selection
                                                                   :selection_mean :selection
                                                                   :selection_burn :selection
                                                                   :selection_sharpe :selection
                                                                   :validation_mean :validation
                                                                   :validation_burn :validation
                                                                   :validation_sharpe :validation
                                                                   :validation_fitness :validation)
                                                   :subtype %1
                                                   :score %2
                                                   :grouping (:grouping e)
                                                   :iter (:iter e))
                                             (keys (dissoc e :iter :grouping :selection_burn)) (vals (dissoc e :iter :grouping  :selection_burn)))) data-V1)}
            :spec {:width 220 :height 500
                   :layer [{:encoding {:x {:field :iter :type "quantitative"}
                                       :y {:field :score :type "quantitative" :scale {:zero false}}
                                       :opacity {:field :grouping :type "nominal" :sort "descending"}
                                       :color {:field :type :type "nominal"}}
                            :mark {:type "line" :tooltip {:content "data"}}}
                           ]}
            :resolve {:scale {:y "independent"}}}]
    V1))

(defn V2-report []
  (let [CR (last @CR-bucket)
        assign (fn [era] (if ((set selection-eras) era) :selection (if ((set buffer-eras) era) :buffer :validation)))
        data-V2 (mapcat (fn [[k v]] (for [st selection-targets tt (list :corr)]
                                      (hash-map :era (Integer/parseInt k)
                                        :score (get-in v [st tt])
                                        :target st
                                        :type (assign k)
                                        :subtarget tt)))
                  (:infer-scores CR))
        V2 {:width 2210 :height 200 :title "NUMERAI CORR scores per selection target"
            :data {:values (filter #(= (:subtarget %) :corr) data-V2)}
            :encoding {:row {:field :target}
                       :x {:field :era :type "ordinal"}
                       :y {:field :score :type "quantitative"}
                       ;; color order is buffer ("#aec7e8"), selection ("#2f88c5"), train ("#0d5592"), tained ("#F8766D"),  validation ("#ff7f0e")
                       :color {:field :type :type "nominal" :scale {:range ["#aec7e8", "#2f88c5", "#F8766D", "#ff7f0e"]}}}
            :mark {:type "bar" :width 2.6 :opacity 0.98 :clip true :tooltip {:content "data"}}}]
    V2))            


;; ## Hillclimbing parameters
;;
;; You want the hillclimber search to accept slight declines, but not
;; immediately. The functions _calc-decline_ and _better-than?_ give you a
;; tailored hill climbing algorithm
;;
(defn calc-decline 
  "% don't work well on a scale when that scale crosses 0. And absolutes need to be re-established each time I change the fitness formula."
  [CR2 ITER]
  (- 0.0055 (min 0.011 (* 0.0005 (- ITER (or (:iter CR2) 0))))))

(defn better-than?
  "Returns true if CR1 has a higher fitness than CR2, given the constraints and number of ITERS since CR2"
  [CR1 CR2 ITER]
  (and CR1 (>= (:selection_fitness CR1) (+ (:selection_fitness CR2) (#'calc-decline CR2 ITER)))))


  
;; ## Main loop
;;
;; Does a lot of things. 
;; 
;; 1. When the current top MUTANT isn't better than CR, evolve a new generation.
;; 2. When the current top MUTANT is better than CR, replace the CR with the freshly trained MUTANT.
;; 3. Put a copy of this new CR in CR-bucket
;; 4. Clear the CODEBLOCKS cache of transcripts that don't occur in the new CR.
;;
(defn do-run [maxiter]

  (while (and continue? (< @ITER maxiter))

    (swap! ITER inc)
    
    ;; When the current top MUTANT isn't better than CR, evolve a new generation.
    (when-not (better-than? (last @MUTANT-POOL) @CR @ITER)
      (doall (cp/pdoseq cpu-threads [PARENT (if (zero? (count @MUTANT-POOL))
                                              (list @CR @CR @CR @CR @CR @CR @CR @CR)
                                              (concat @MUTANT-POOL @MUTANT-POOL @MUTANT-POOL @MUTANT-POOL))]
               (swap! MUTANT-POOL conj (#'do-train (#'do-mutate PARENT)))))
      (reset! MUTANT-POOL (take-last 2 (sort-by #(:selection_fitness %) @MUTANT-POOL))))

    (when-not (better-than? (last @MUTANT-POOL) @CR @ITER)
      (println (format "       %+.4f < %.4f%+.4f, [%03.1f secs]" (:selection_fitness (last @MUTANT-POOL)) (:selection_fitness @CR) (#'calc-decline @CR @ITER) (/ (double (- (. System (nanoTime)) @TIMER)) 1000000000.0)))
      (reset! TIMER (. System (nanoTime))))

    ;; Replace the CR when the freshly trained MUTANT is better than the current CR
    (when (better-than? (last @MUTANT-POOL) @CR @ITER)
      (println (format "%4d   %+.4f > %.4f%+.4f, [%03.1f secs]" @ITER (:selection_fitness (last @MUTANT-POOL)) (:selection_fitness @CR) (#'calc-decline @CR @ITER) (/ (double (- (. System (nanoTime)) @TIMER)) 1000000000.0)))
      (reset! TIMER (. System (nanoTime)))
      ;;
      ;; update CR and report.
      (reset! CR (assoc (last @MUTANT-POOL) :iter @ITER))
      (swap! CR-bucket conj @CR)
      (when (zero? (mod (count @CR-bucket) 10))
        (oz/view! 
          [:div {:style {:display "flex" :flex-direction "column"}}
           [:vega-lite (#'V1-report)]
           [:vega-lite (#'V2-report)]]))
      ;;
      (reset! MUTANT-POOL [])
      (let [obsolete (clojure.set/difference (set (keys @CODEBLOCKS)) (set (:transcript @CR)))]
        (reset! CODEBLOCKS (zipmap (keys @CODEBLOCKS) (mapv (fn [[k v]] (if (obsolete k) (release v) v)) @CODEBLOCKS)))
        (reset! CODEBLOCKS (postwalk identity (into {} (remove (fn [[_ v]] (boolean? v)) @CODEBLOCKS))))
        ))

    (when (zero? (mod @ITER 100))
      ;; Ever so often reset the CODEBLOCKS pool, to avoid it becoming too large during long searches for an improvement      
      (let [obsolete (clojure.set/difference (set (keys @CODEBLOCKS)) (set (:transcript @CR)))]
        (reset! CODEBLOCKS (zipmap (keys @CODEBLOCKS) (mapv (fn [[k v]] (if (obsolete k) (release v) v)) @CODEBLOCKS)))
        (reset! CODEBLOCKS (postwalk identity (into {} (remove (fn [[_ v]] (boolean? v)) @CODEBLOCKS))))))
    ))

;; ## Store and Retrieve functions
;;
(defn store-POOL [POOL filename] (freeze-to-file filename (mapv #(dissoc % :code) POOL)))


;; ## Main function to start a run.
(defn train-fold [options]

  ;(def options {:firstbufferera "0885" :numiters 3000})  

  (def init (atom (:firstbufferera options)))
  (def validation-eras (vec (take 170 (drop 15 (drop-while #(not= % @init) all-eras)))))
  (def buffer-eras (vec (concat (take 15 (drop-while #(not= % @init) all-eras)) (take-last 15 (take 200 (drop-while #(not= % @init) all-eras))))))
  (def selection-eras (vec (set/difference (set all-eras) (set validation-eras) (set buffer-eras))))
  
  ;; Targets
  (def selection-targets ["cyrus-truth" "merged-truth"])
  (def offsets-map {"cyrus-truth" {:corr 0.0} "merged-truth" {:corr -0.015}})

  
  ;; Resets
  (reset! ITER 0)
  (reset! CR-bucket [])
  (reset! MUTANT-POOL [])

  ;; Start
  (def rand-ID (rand-int 10000))
  (generate-adam)
  (swap! CR-bucket conj (assoc @CR :selection-eras selection-eras :validation-eras validation-eras :buffer-eras buffer-eras))
  
  ;; Continue
  (def continue? true)
  (do-run (:numiters options))
  ;;(def continue? false)

  (def filename "training_results/mymodel")
  ;; We need to split up CR-bucket to smaller chunks as the number of transcripts in CR-bucket gets larger.
  ;; The reason for not just simply dissoc'ing :transcript and storing a smaller file is that 
  ;; these files are independent of how the buildCR genome-to-transcript code works.
  (doseq [[chunk num] (map vector (partition-all 1000 @CR-bucket) (range))]
    (store-POOL chunk (str filename "-rseed" rand-ID "-firstbufferera" (:firstbufferera options) "-numiters" @ITER "-chunk" num ".nippy")))
  (do (oz/export! [:div {:style {:display "flex" :flex-direction "column"}}
                   [:vega-lite (#'V1-report)]
                   [:vega-lite (#'V2-report)]]
        (str filename "-rseed" rand-ID "-firstbufferera" (:firstbufferera options) "-numiters" @ITER ".html")) nil)
  
  (let [obsolete (set (keys @CODEBLOCKS))]
      (reset! CODEBLOCKS (zipmap (keys @CODEBLOCKS) (mapv (fn [[k v]] (if (obsolete k) (release v) v)) @CODEBLOCKS)))
      (reset! CODEBLOCKS (postwalk identity (into {} (remove (fn [[_ v]] (boolean? v)) @CODEBLOCKS)))))
  )

(defn continue-fold [options]

  ;(def options {:firstbufferera "0885" :oldnumiters 5000 :numiters 25000 :rseed 7422}) 

  (def init (atom (:firstbufferera options)))
  (def validation-eras (vec (take 170 (drop 15 (drop-while #(not= % @init) all-eras)))))
  (def buffer-eras (vec (concat (take 15 (drop-while #(not= % @init) all-eras)) (take-last 15 (take 200 (drop-while #(not= % @init) all-eras))))))
  (def selection-eras (vec (set/difference (set all-eras) (set validation-eras) (set buffer-eras))))
  
  ;; Targets
  (def selection-targets ["cyrus-truth" "merged-truth"])
  (def offsets-map {"cyrus-truth" {:corr 0.0} "merged-truth" {:corr -0.015}})

  
  ;; Resets
  (reset! CR-bucket [])
  (reset! MUTANT-POOL [])

  ;; Continue
  (def rand-ID (:rseed options))
  (let [filenames (->> (seq (.list (clojure.java.io/file "training_results/")))
                    (filter #(.contains % "chunk"))
                    (filter #(.contains % (str "rseed" (:rseed options))))
                    (filter #(.contains % (str "numiters" (:oldnumiters options))))
                    (sort-by #(parse-long (second (re-find #"chunk(\d+)\.nippy" %)))))]
    (reset! CR-bucket (reduce into [] (map #(thaw-from-file (str "training_results/" %)) filenames))))
  (reset! CR (last @CR-bucket))
  (reset! ITER (:iter @CR))
  
  
  ;; Continue
  (def continue? true)
  (do-run (:numiters options))
  ;;(def continue? false)

  (def filename "training_results/mymodel")
  ;; We need to split up CR-bucket to smaller chunks as the number of transcripts in CR-bucket gets larger.
  ;; The reason for not just simply dissoc'ing :transcript and storing a smaller file is that 
  ;; these files are independent of how the buildCR genome-to-transcript code works.
  (doseq [[chunk num] (map vector (partition-all 1000 @CR-bucket) (range))]
    (store-POOL chunk (str filename "-rseed" rand-ID "-firstbufferera" (:firstbufferera options) "-numiters" @ITER "-chunk" num ".nippy")))
  (do (oz/export! [:div {:style {:display "flex" :flex-direction "column"}}
                   [:vega-lite (#'V1-report)]
                   [:vega-lite (#'V2-report)]]
        (str filename "-rseed" rand-ID "-firstbufferera" (:firstbufferera options) "-numiters" @ITER ".html")) nil)
  
  (let [obsolete (set (keys @CODEBLOCKS))]
      (reset! CODEBLOCKS (zipmap (keys @CODEBLOCKS) (mapv (fn [[k v]] (if (obsolete k) (release v) v)) @CODEBLOCKS)))
      (reset! CODEBLOCKS (postwalk identity (into {} (remove (fn [[_ v]] (boolean? v)) @CODEBLOCKS)))))
  )
