# Engineer

Engineer is a Genetic Programming Algorithm written in Clojure that generates solutions for the [https://numer.ai](Numer.ai machine learning competition). 

## Overview 

This GPA generates a random vector of features and transformations (the genome of the original critter) whch gets translated into an ensemble of functions. These functions are a combination of decision tree and feature engineering elements, and the predictions of each of these functions is averaged into a final prediction.

The below string is part of such a genome. It gets broken into chunks by the :break statements, and then translated into functions.

```
[nil f3 :min f4 :<=! f6 f1 f9 :break :!min :> :!max :!min f0 nil f8 nil :<! :!<= nil nil :!max :> :<=! f5 f7 ... 
```

For example, the start of this genome, _"nil f3 :min f4 :<=! f6 f1 f9"_, would be translated into the following function:

```
1. ignore the first two symbols, as they are not functions.

2. compute the _rowwise min_ of:
	* feature 4
	* compute the _rowwise if_ of: if (feature 6 < inverse of feature 1) then return feature 9, else return 0.0)

``` 				

The fuzzy hillclimbing algorithm iteratively searches for mutations that improve the critter by changing the genome in various ways. These mutations help the offspring of the critter to explore both evolutionary "nearby" and "further away" solutions. Once you have trained a few folds, you can let Engineer merge the best of these critters into an ensemble that converts the live feature data into predictions, which it then can submit to numer.ai.


## Quickstart

* Have an Intel or AMD processor (to run the Intel MKL libraries)

* Install [Clojure](https://clojure.org/guides/install_clojure)

* Download the release and unpack it in a directory of your choice

* Move the v42correlations.nippy file in the /data directory

* (you can skip this step, as the training results are included) Train the model (clj -M:train)

* (you can skip this step, as the merge results are included) Merge the model predictions (clj -M:merge)

* Fetch the live data and build the predictions (clj -M:predict)

* Provide an API key with upload and upload history permissions, and a model ID key in the /secret directory.

* Submit the predictions (clj -M:submit)

* Run polly, who constantly polls numer.ai to see if a new round has started, and then predicts and submits for you (clj -M:polly)



## Helpful links

A description of what all the files do is here: [FILES.md](FILES.md)

You can look at the performance of this model here: [engineer_public_rv1](https://numer.ai/engineer_public_rv1). Currently not very informative, as this release version has only been submitting for a few days now. 

## Disclaimer

The information and code provided in this GitHub repository are for educational and entertainment purposes only. Any information in this repository is not intended to be used as financial advice, and the owner(s) and contributor(s) of this repository are not financial advisors. The owner(s) and contributor(s) of this repository do not guarantee the accuracy or completeness of the information provided, and they are not responsible for any losses or damages that may arise from the use of this information or code.