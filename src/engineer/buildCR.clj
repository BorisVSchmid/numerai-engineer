(ns engineer.buildCR
  (:require [clojure.java.io :as io]))


;; ## Constants
;;
(def data_dir "data")

(def list-features (mapv #(list (symbol "taoensso.nippy" "thaw-from-file") (str data_dir "/training/" %))
         (filter #(.contains % "feature_") (seq (.list (clojure.java.io/file (str data_dir "/training")))))))
  
(def constant-zero (list (symbol "taoensso.nippy" "thaw-from-file") (str data_dir "/training/feature_zero_point_zero.nippy")))

;; While it looks like I don't call these, I do (constructed calls)
(def list-build-features list-features)
(def list-empty [nil])
(def list-signals [:break])
;; removed functionally identical duplicates.
(def list-operators [:< :!< :<! :> #_:!> #_:>! :<= :!<= :<=! :>= #_:!=> #_:=>! :min :!min :min! :max! :!max :max :flip]) ;; those commented out were equivalent when using a -0.5 / 0.5 scheme. Probably still equivalent in more complex schemes


;; ## Core functions to build and interpret genomes
;;
(def default-params
  (hash-map
   :genome-length [500]
   :genome-ratios (vec (concat (repeat 15 (symbol "engineer.buildCR" "list-build-features"))
                               (repeat 5  (symbol "engineer.buildCR" "list-empty"))
                               (repeat 1  (symbol "engineer.buildCR" "list-signals"))
                               (repeat 5  (symbol "engineer.buildCR" "list-operators"))))
   :mutate-ratios (vec (concat
                        ;; sequence (76x)
                        (repeat 10 (symbol "engineer.mutateCR" "mutate"))
                        (repeat 10 (symbol "engineer.mutateCR" "shift"))
                        (repeat 10 (symbol "engineer.mutateCR" "switch"))
                        (repeat 10 (symbol "engineer.mutateCR" "insert"))
                        (repeat 10 (symbol "engineer.mutateCR" "delete"))
                        (repeat 10  (symbol "engineer.mutateCR" "swap-block"))
                        (repeat 10 (symbol "engineer.mutateCR" "duplicate-block"))
                        (repeat 10 (symbol "engineer.mutateCR" "delete-block"))))))


(defn new-genome [length ratios]
  (vec (repeatedly length #(rand-nth (eval (rand-nth ratios))))))


(defn break-and-clean-genome
  "Translates the genome into separate chunks, and cleans up non-functional chunks and bits of chunks."
  [genome]
  (->>
   (partition-by #(= % :break) genome)                                              ;; Split by :break signals
   (remove #(contains? (set %) :break))                                             ;; Remove :break (and sequential [:break :break ..]) signals
   (map #(remove nil? %))                                                           ;; Remove nils from within chunks
   (map #(drop-while (fn [n] (not (keyword? n))) %))                                ;; Drop non-operators at start of a chunk.
   (map #(reverse (drop-while (fn [n] (keyword? n)) (reverse %))))                  ;; Drop operators at the end of a chunk.
   (filter #(and (some keyword? %) (> (count (set (map type %))) 1)))               ;; Keep chunks that hold at least 1 operator and 1 non-operator
   (remove empty?)))                                                                ;; Remove empty chunks


(defn split-by-keyword
  "splits a chunk of DNA into smaller seqs, each time you find a keyword"
  [coll]
  (loop [C coll temp [] bucket []]
    (if (seq C)
      (recur (rest C)
             (if (keyword? (first C)) [(first C)] (conj temp (first C)))
             (if (keyword? (first C)) (conj bucket temp) bucket))
      (remove empty? (conj bucket temp)))))


(defn resolver
  "Translates a keyword and its arguments into code. functions to translate are: list-operators [:< :!< :<! :> :<= :!<= :<=! :>= :min :!min :min! :max! :!max :max :flip.  
   NOT :!> :>! :!=> :=>!, which are duplicates of other operators. 
   Also, as long as we don't have embedded layers, and everything is -0.5, 0.0 or 0.5, the trigonomy functions are mostly a minor variant of identity, so not useful."
  [coll]
  (let [function (first coll)
        arguments (rest coll)]
    (case function
      :flip  (case (count arguments)
               0 constant-zero
               (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (first arguments))) ;; just flips the first argument if there are many. No summing.
      :< (case (count arguments)
           0 constant-zero
           1 constant-zero
           2 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b) (list 'if (list '< 'a 'b) -0.5 0.5)) (first arguments) (second arguments))
           3 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c) (list 'if (list '< 'a 'b) 'c 0.0)) (first arguments) (second arguments) (last arguments))
           (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c 'd) (list 'if (list '< 'a 'b) 'c 'd)) (first arguments) (second arguments) (nth arguments 2) (nth arguments 3))) ;; ignoring more than 4 arguments.
      :!< (case (count arguments)
            0 constant-zero
            1 constant-zero
            2 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b) (list 'if (list '< 'a 'b) -0.5 0.5)) (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (first arguments)) (second arguments))
            3 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c) (list 'if (list '< 'a 'b) 'c 0.0)) (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (first arguments)) (second arguments) (last arguments))
            (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c 'd) (list 'if (list '< 'a 'b) 'c 'd)) (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (first arguments)) (second arguments) (nth arguments 2) (nth arguments 3))) ;; ignoring more than 4 arguments      
      :<! (case (count arguments)
            0 constant-zero
            1 constant-zero
            2 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b) (list 'if (list '< 'a 'b) -0.5 0.5)) (first arguments) (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (second arguments)))
            3 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c) (list 'if (list '< 'a 'b) 'c 0.0)) (first arguments) (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (second arguments)) (last arguments))
            (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c 'd) (list 'if (list '< 'a 'b) 'c 'd)) (first arguments) (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (second arguments)) (nth arguments 2) (nth arguments 3))) ;; ignoring more than 4 arguments.           
      :> (case (count arguments)
           0 constant-zero
           1 constant-zero
           2 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b) (list 'if (list '> 'a 'b) -0.5 0.5)) (first arguments) (second arguments))
           3 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c) (list 'if (list '> 'a 'b) 'c 0.0)) (first arguments) (second arguments) (last arguments))
           (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c 'd) (list 'if (list '> 'a 'b) 'c 'd)) (first arguments) (second arguments) (nth arguments 2) (nth arguments 3))) ;; ignoring more than 4 arguments.
      :<= (case (count arguments)
            0 constant-zero
            1 constant-zero
            2 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b) (list 'if (list '<= 'a 'b) -0.5 0.5)) (first arguments) (second arguments))
            3 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c) (list 'if (list '<= 'a 'b) 'c 0.0)) (first arguments) (second arguments) (last arguments))
            (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c 'd) (list 'if (list '<= 'a 'b) 'c 'd)) (first arguments) (second arguments) (nth arguments 2) (nth arguments 3))) ;; ignoring more than 4 arguments.
      :!<= (case (count arguments)
             0 constant-zero
             1 constant-zero
             2 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b) (list 'if (list '<= 'a 'b) -0.5 0.5)) (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (first arguments)) (second arguments))
             3 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c) (list 'if (list '<= 'a 'b) 'c 0.0)) (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (first arguments)) (second arguments) (last arguments))
             (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c 'd) (list 'if (list '<= 'a 'b) 'c 'd)) (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (first arguments)) (second arguments) (nth arguments 2) (nth arguments 3))) ;; ignoring more than 4 arguments.
      :<=! (case (count arguments)
             0 constant-zero
             1 constant-zero
             2 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b) (list 'if (list '<= 'a 'b) -0.5 0.5)) (first arguments) (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (second arguments)))
             3 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c) (list 'if (list '<= 'a 'b) 'c 0.0)) (first arguments) (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (second arguments)) (last arguments))
             (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c 'd) (list 'if (list '<= 'a 'b) 'c 'd)) (first arguments) (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (second arguments)) (nth arguments 2) (nth arguments 3))) ;; ignoring more than 4 arguments.
      :>= (case (count arguments)
            0 constant-zero
            1 constant-zero
            2 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b) (list 'if (list '>= 'a 'b) -0.5 0.5)) (first arguments) (second arguments))
            3 (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c) (list 'if (list '>= 'a 'b) 'c 0.0)) (first arguments) (second arguments) (last arguments))
            (list (symbol "uncomplicate.fluokitten.core" "fmap!") (list 'fn (vector 'a 'b 'c 'd) (list 'if (list '>= 'a 'b) 'c 'd)) (first arguments) (second arguments) (nth arguments 2) (nth arguments 3))) ;; ignoring more than 4 arguments.
      :min (case (count arguments)
             0 constant-zero
             1 (first arguments)
             (list (symbol "uncomplicate.neanderthal.vect-math" "fmin!") (first arguments) (second arguments)))
      :!min (case (count arguments)
              0 constant-zero
              1 (first arguments)
              (list (symbol "uncomplicate.neanderthal.vect-math" "fmin!") (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (first arguments)) (second arguments)))
      :min! (case (count arguments)
              0 constant-zero
              1 (first arguments)
              (list (symbol "uncomplicate.neanderthal.vect-math" "fmin!") (first arguments) (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (second arguments))))
      :max (case (count arguments)
             0 constant-zero
             1 (first arguments)
             (list (symbol "uncomplicate.neanderthal.vect-math" "fmax!") (first arguments) (second arguments)))
      :!max (case (count arguments)
             0 constant-zero
             1 (first arguments)
             (list (symbol "uncomplicate.neanderthal.vect-math" "fmax!") (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (first arguments)) (second arguments)))
      :max! (case (count arguments)
             0 constant-zero
             1 (first arguments)
             (list (symbol "uncomplicate.neanderthal.vect-math" "fmax!") (first arguments) (list (symbol "uncomplicate.neanderthal.core" "scal!") -1.0 (second arguments)))))))


(defn backward-resolver
  "Solves a chunk of DNA."
  [coll]
  (loop [C (reverse (butlast coll)) O (resolver (last coll))]
    (if (seq C)
      (recur (rest C) (resolver (conj (first C) O)))
      (list 'fn [] O))))


(defn gen-transcript
  "Turn a genome string into a collection of formulas. Generates at least a single formula"
  [genome]
  (let [transcript (->> (break-and-clean-genome genome)
                        (map split-by-keyword)
                        (mapv backward-resolver))]
    (if (seq transcript) (set transcript)
        (vector (list 'fn [] constant-zero)))))


(defn build-CR [params]
  (let [genome (new-genome (rand-nth (:genome-length params)) (:genome-ratios params))
        transcript (gen-transcript genome)]
    (hash-map :genome genome
              :transcript transcript
              :last-mutation-set #{:genesis}
              :iter 0
              :fitness -10.0)))