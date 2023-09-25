(ns engineer.mutateCR
  (:require [clojure.set])
  (:require [clojure.string :as string])
  (:require [taoensso.nippy :refer [thaw-from-file]])
  (:require [engineer.buildCR :refer [default-params constant-zero gen-transcript list-features]]))



;; ## Constants
;;
(def data_dir "data")
(def related-features (thaw-from-file (str data_dir "/v42correlations.nippy")))



;; ## Helper functions
;;
(defn translate-featurelist-to-feature [featurelist]
  (str "feature" (second (string/split (second featurelist) #"feature"))))

(defn translate-feature-to-featurelist [feature]
  (let [operator (ffirst list-features)
        prelude (first (string/split (second (first list-features)) #"feature"))]
    (list operator (str prelude feature))))



;; ## Functions that mutate the params or the genetic code of the CR
;;
(defn mutate
  "Mutates one element, somewhere in the genome."
   [CR]
  (let [genome (:genome CR)
        pos (rand-int (count genome))
        replacement (rand-nth (eval (rand-nth (:genome-ratios default-params))))]
    (assoc CR :genome (vec (concat (subvec genome 0 pos) (list replacement) (rest (subvec genome pos)))) :last-mutation-set #{:mutate})))

(defn shift
  "shifts a feature to a nearby correlated feature (80% pearson). If that doesn't exist, take a randome feature"
  [CR]
  (let [genome (:genome CR)
        pos (first (rand-nth (filter last (map #(list %1 (seq? %2)) (range) genome))))
        new-candidate-feats (vec (clojure.set/intersection (set list-features) (set (related-features (translate-featurelist-to-feature (genome pos))))))
        new-feat (if (empty? new-candidate-feats) (rand-nth list-features) (translate-feature-to-featurelist (rand-nth new-candidate-feats)))
        new (assoc genome pos new-feat)]
    (assoc CR :genome new :last-mutation-set (if (empty? new-candidate-feats) (set (list :switch new-feat)) (set (list :shift new-feat))))))

(defn switch
  "similar to shift, but switches a feature to a random other feature"
   [CR]
  (let [genome (:genome CR)
        pos (first (rand-nth (filter last (map #(list %1 (seq? %2)) (range) genome)))) ;; this breaks if a genome doesn't have any features left in it.
        new-feat (rand-nth list-features)
        new (assoc genome pos new-feat)]
    (assoc CR :genome new :last-mutation-set (set (list :switch new-feat)))))

(defn insert
  "Inserts a few elements, somewhere in the genome."
   [CR]
  (let [genome (:genome CR)
        pos (rand-int (count genome))
        insertion (repeatedly (inc (rand-int 40)) #(rand-nth (eval (rand-nth (:genome-ratios default-params)))))]
    (assoc CR :genome (vec (concat (subvec genome 0 pos) insertion (subvec genome pos))) :last-mutation-set #{:insert})))

(defn delete
  "Deletes a few elements, somewhere in the genome."
   [CR]
  (let [genome (:genome CR)
        deletion (min (count (:genome CR)) (inc (rand-int 40)))
        pos (rand-int (- (count genome) deletion))]
    (assoc CR :genome (vec (concat (subvec genome 0 pos) (subvec genome (+ pos deletion)))) :last-mutation-set #{:delete})))

(defn alter-gene [gene CR]
  (let [alteration (rand-nth (filter #{(symbol "engineer.mutateCR" "mutate")
                                       (symbol "engineer.mutateCR" "switch")
                                       (symbol "engineer.mutateCR" "insert")
                                       (symbol "engineer.mutateCR" "delete")} (:mutate-ratios default-params)))]
    (select-keys ((eval alteration) (assoc CR :genome gene)) [:last-mutation-set :genome])))

(defn swap-block
  "Swaps the order of two code blocks, and alters one of the blocks. We take interesting blocks."
   [CR]
  (if (> (count (:transcript CR)) 1)
    (let [genome (:genome CR)
          blocks (zipmap (range) (partition-by #(= % :break) genome))
          b1 (first (drop-while #(= (gen-transcript (blocks %)) (vector (list 'fn [] constant-zero))) (shuffle (keys blocks))))
          b2 (first (drop-while #(= (gen-transcript (blocks %)) (vector (list 'fn [] constant-zero))) (shuffle (keys blocks))))
          newblock (alter-gene (vec (blocks b1)) CR)
          swapped-blocks (assoc blocks b1 (blocks b2) b2 (:genome newblock))
          new (reduce into [] (map swapped-blocks (sort (keys blocks))))]
      (assoc CR :genome new :last-mutation-set (clojure.set/union (:last-mutation-set newblock) #{:swap-block})))
    (assoc CR :last-mutation-set #{:swap-block})))

(defn duplicate-block
  "Duplicates a code block, and alters the duplicate, and places it back next to the original block. We take interesting blocks."
   [CR]
  (let [genome (:genome CR)
        blocks (zipmap (range) (partition-by #(= % :break) genome))
        newblock (first (drop-while #(= (gen-transcript (blocks %)) (vector (list 'fn [] constant-zero))) (shuffle (keys blocks))))
        newgene (alter-gene (vec (blocks newblock)) CR)
        blocks (assoc blocks (+ newblock (rand)) (:genome newgene)) ;; insert the duplicate before or after the original block
        new (reduce into [] (map blocks (sort (keys blocks))))]
    (assoc CR :genome new :last-mutation-set (clojure.set/union (:last-mutation-set newgene) #{:duplicate-block}))))

(defn delete-block
  "Deletes a code block, if there is more than 1 left. We pick interesting blocks, i.e., those that generate a nontrivial transcript"
   [CR]
  (-> (if (> (count (:transcript CR)) 1)
        (let [genome (:genome CR)
              blocks (zipmap (range) (partition-by #(= % :break) genome))
              deleted (first (shuffle (keys blocks)))
              new (reduce into [] (map blocks (remove #(= deleted %) (sort (keys blocks)))))]
          (assoc CR :genome new))
        CR)
      (assoc :last-mutation-set #{:delete-block})))
