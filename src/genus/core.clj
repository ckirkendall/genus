(ns genus.core
  (:require [clojure.string :as str]
            [genus.model :as model]))

(def min-prob 0.000001)

(defn posterior-prob
  "for each classification this
   returns posterior probability
   based on the bayesian inference
   formula
   p(H|E) = p(E|H) * P(H) / P(E)"
  [{:keys [classes all]} prior-prob word]
  (let [start-prob (/ 1 (count classes))]
    (into {}
          (for [[class word-probs] classes]
            (let [pe (get all word min-prob)
                  peh (get word-probs word min-prob)
                  ph (get prior-prob class start-prob)]
              [class (/ (* peh ph) pe)])))))


(defn bayesian-inference [model words]
  (reduce (partial posterior-prob model)
          {}
          words))


(defn classify
  ([model content]
   (classify model content model/default-tokenizer))
  ([model content tokenizer]
   (let [words (tokenizer content)
         probs (bayesian-inference model words)
         [[class prob]] (reverse (sort-by second probs))]
     (if (>= prob (get-in model [:thresholds class] min-prob))
       class
       :unknown))))
