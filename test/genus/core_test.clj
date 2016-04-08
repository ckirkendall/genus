(ns genus.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [genus.core :refer :all]
            [genus.model :as model]))

(deftest prio-prob-test
  (let [start 0.5
        model (as-> {} m
                (model/teach :c1 m [:a :b])
                (model/teach :c2 m [:b :c])
                (model/normalize-model m))]
    (is (= 1 (:c1 (posterior-prob model start :a))))
    (is (= (* min-prob 2) (:c2 (posterior-prob model start :a))))
    (is (= 1/2 (:c1 (posterior-prob model start :b))))))

(deftest basic-classify-test
  (let [config {:training-data
                {:cat1 (io/resource "model_test/cat1.txt")
                 :cat2 (io/resource "model_test/cat2.txt")}}
        model (model/create-model config)
        class1 (classify model "red blue red yellow red")
        class2 (classify model "blue red blue yellow blue")]
    (is (= :cat1 class1)
        (= :cat2 class2))))


(deftest shakespeare-dickens-test
  (let [config {:training-data
                {:dickens
                 [(io/resource "dickens/hard-times.txt")
                  (io/resource "dickens/olivertwist.txt")
                  (io/resource "dickens/twocities.txt")]
                 :shakespear
                 [(io/resource "shakespeare/hamlet.txt")
                  (io/resource "shakespeare/macbeth.txt")
                  (io/resource "shakespeare/midsummer.txt")]}}
        model (model/create-model config)
        sample1 (->> (io/resource "samples/great-exp-sample.txt")
                     io/reader
                     line-seq
                     (interpose " ")
                     (apply str))]
    (is (= :dickens (classify model sample1)))))
