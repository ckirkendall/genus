(ns genus.core
  (:require [clojure.string :as str]))

(def min-prob 0.00001)

(defn posterior-prob
  "for each classification this
   returns posterior probability
   based on the bayesian inference
   formula
   p(H|E) = p(E|H) * P(H) / P(E)"
  [{:keys [classes all]} prior-prob word]
  (let [start-prob (/ 1 (count classes))]
    (into {}
          (for [class classes]
            (let [pe (get all word min-prob)
                  peh (get class word min-prob)
                  ph (get prior-prob class start-prob)]
              [class (/ (* peh ph) pe)])))))


(defn bayesian-inference [model words]
  (reduce (partial posterior-prob model)
          {}
          words))


(defn classify [model line]
  (let [words (str/split line #" |\.|:|,|!|;")
        probs (bayesian-inference model words)
        [[class prob]] (reverse (sort-by second probs))]
    (if (>= prob (get-in model [:thresholds class]))
      class
      :unknown)))
