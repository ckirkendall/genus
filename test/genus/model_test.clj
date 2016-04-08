(ns genus.model-test
  (:require [genus.model :refer :all]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(deftest model-tets
  (let [config {:training-data
                {:cat1 (io/resource "model_test/cat1.txt")
                 :cat2 (io/resource "model_test/cat2.txt")}}
        model (create-model config)]
    (is (= {:total-words 29
            :all {"red" 11/29, "blue" 11/29, "green" 4/29, "yellow" 3/29}
            :classes {:cat1 {"red" 8/15, "blue" 1/5, "green" 2/15, "yellow" 2/15}
                      :cat2 {"blue" 4/7, "red" 3/14, "yellow" 1/14, "green" 1/7}}
            :tokenizer nil :thresholds nil}
           model))))
