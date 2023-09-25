### Files

Here is a description of the different files in the /src directory.

#### For training



**fetch.clj**: This file handles data retrieval and preparation. It fetches the latest data for the competition, transforms it to a range of -1.64 to 1.64, replaces any missing feature data with 0.0, and stores it in a format that can be quickly read from an SDD drive. The .nippy format is quick enough that Engineer doesn't have to keep the whole dataset in memory (_it still caches active features_). **Fetch.clj** also calculates a correlation table between the features based on the new data. 



**buildCR.clj**: This file is responsible for constructing the individual critters, which are the potential solutions to the competition's problem. It provides the mechanisms to create a new linear genome, and to translate this genome into a collection of decision tree plus feature engineering functions. 

Critters can evolve dozens to hundreds of these small functions, which all get summed up into the final solution. Earlier versions of Engineer would feed these dozens of small functions into a neural network which would subsequently get trained (_which is also the legacy reason why the feature categories get translated to a gaussian_), but that came at a considerable extra compute cost without clear benefits.



**mutateCR.clj**: This file implements a set of mutations to evolve critter genomes. It balances both minor and major transformations to introduce a mix of incremental and drastic changes. Minor transformations include tweaking single elements or swapping features for related features, adding a fine-grained exploration of the solution space. Major transformations —such as inserting or deleting larger blocks of genome— can significantly alter the phenotype of the critter, enabling a broad exploration of new and potentially more effective solutions.



**hillclimber.clj**: This file implements the core logic of the evolutionary process. It trains the critters, generates mutants, evaluate those mutants and selects the best with a fuzzy hill climbing algorithm. Hillclimber also generates a visual performance report, and stores the result of the hill climbing in **/training_results**

In more detail: the algorithm works by iteratively exploring and evaluating potential modifications ("mutations") to a current solution to maximize a fitness function. The fitness function is a combination of the mean and sharpe of the _numer.ai correlation_ scores of individual eras with the target. It prefers solutions with more stable performance across different eras (due to the sharpe) and different targets (because the lowest per-era correlation score across targets is taken as the era score).

The hillclimber code is designed to be flexible with respect to the target variables it is optimizing for. In this release, the targets are "cyrus-truth" and "merged-truth", with the "merged-truth" being a combination of several recent targets to help fit the "cyrus-truth" more smoothly.



**merger.clj**: This file holds the functions responsible for the selection, merging, and ensembling of the evolved critters. It selects the top 20 critters with the highest mean score on the validation data, that evolved at least 50 timesteps apart. The mean and sharpe ratio of this ensemble is calculated to assess its performance. This ensemble is stored as a single .nippy file in the **/merger_results** directory.

The selection comes with a visual report feature, mainly for user supervision, offering a quick and intuitive check on the system's subsetting algorithm and performance.



**stats.clj**: This file contains some statistical functions as well as the evaluation function used by Numer.ai.




#### For daily submission


**predict.clj**: This file  does some minor transformations to prepare the data for making valid predictions, and then generates predictions from the ensembled subset of Critters stored in /stable.


**submit.clj**: This file manages the submission of solutions to the competition. It prepares the data in the required format, sends it to the competition's servers, and confirms the submission.


**core_something.clj**: These are the entry points for developing with Engineer from the command-line. When you type **clj -M:fetch**, the _core_fetch.clj_ file is executed. The same goes for _core_train.clj_, _core_merge.clj_, _core_predict.clj_ (which fetches the live data and generates predictions), and _core_submit.clj_ to submit the prediction, which respond to **clj -M:train**, **-M:merge**, **-M:predict** and **-M:submit**. Once you are happy with the process, you can automate it with _polly.clj_, which will continuously poll the numer.ai API and submit your predictions when the new round opens. For submitting live submissions, you need to update the dummy api keys stored in **/secret**.


