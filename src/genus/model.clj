(ns genus.model
  (:require [cognitect.transit :as transit]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.xml :as xml])
  (:import [java.io BufferedReader StringReader]))

(defn default-tokenizer [content]
  (let [lines (BufferedReader. (StringReader. content))]
    (mapcat #(str/split % #" |\.|:|,|!|;"))))


(defn get-tokenizer [{:keys [tokenizer]} class]
  (if (fn? tokenizer)
    tokenizer
    (get tokenizer class default-tokenizer)))


(defn normalize-class [classes total-words]
  (into {} (for [[k word-cnts] classes]
             [k (into {} (for [[word cnt] word-cnts]
                           [k (/ cnt total-words)]))])))


(defn normalize-model [{:keys [total-words all classes tokenizer]}]
  {:total-words total-words
   :all (normalize-class {:all all} total-words)
   :classes (normalize-class classes total-words)
   :tokenizer tokenizer})


(defn add-token [class model token]
  (-> model
      (update-in [:classes class token] (fnil inc 0))
      (update-in [:all token] (fnil inc 0))
      (update :total-words (fnil inc 0))))


(defn teach [class model tokens]
  (reduce (partial add-token class)
          model
          tokens))


(defn add-file [model [class file-name]]
  (let [content (slurp file-name)
        token-fn (get-tokenizer model)]
    (reduce (partial teach class)
            model
            (token-fn content))))


(defn create-model [config out-file-name]
  (let [init (select-keys [:tokenizer])
        model (-> (reduce add-file init (:training-data config))
                  (normalize-model)
                  (assoc :thresholds (:thresholds config)))]
    (with-open [out (io/output-stream out-file-name)]
      (let [writer (transit/writer out :json)]
        (transit/write writer model)))))


(defn load-model [model-file-name]
  (with-open [in (io/input-stream model-file-name)]
    (let [reader (transit/reader in :json)]
      (transit/read reader))))
