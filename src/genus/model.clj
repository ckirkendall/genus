(ns genus.model
  (:require [cognitect.transit :as transit]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.xml :as xml])
  (:import [java.io BufferedReader StringReader]))

(defn default-tokenizer [content]
  (let [lines (line-seq (BufferedReader. (StringReader. content)))]
    (mapcat #(str/split % #" |\.|:|,|!|;") lines)))


(defn get-tokenizer [{:keys [tokenizer]} class]
  (if (fn? tokenizer)
    tokenizer
    (get tokenizer class default-tokenizer)))


(defn normalize-class [classes]
  (into {} (for [[k word-cnts] classes]
             (let [total (::total word-cnts)]
               [k (into {} (for [[word cnt] (dissoc word-cnts ::total)]
                             [word (/ cnt total)]))]))))


(defn normalize-model [{:keys [total-words all classes tokenizer]}]
  {:total-words total-words
   :all (zipmap (keys all) (map #(/ % total-words) (vals all)))
   :classes (normalize-class classes)
   :tokenizer tokenizer})


(defn add-token [class model token]
  (-> model
      (update-in [:classes class token] (fnil inc 0))
      (update-in [:classes class ::total] (fnil inc 0))
      (update-in [:all token] (fnil inc 0))
      (update :total-words (fnil inc 0))))


(defn teach [class model tokens]
  (reduce (partial add-token class)
          model
          tokens))


(defn add-file [class model file-name]
  (let [content (slurp file-name)
        token-fn (get-tokenizer model class)]
    (teach class model (token-fn content))))


(defn add-files [model [class file-names]]
  (let [files (if (coll? file-names)
                file-names
                [file-names])]
    (reduce (partial add-file class) model files)))


(defn create-model [config]
  (let [init (select-keys config [:tokenizer])]
    (-> (reduce add-files init (:training-data config))
        (normalize-model)
        (assoc :thresholds (:thresholds config)))))


(defn write-model [model out-file-name]
  (with-open [out (io/output-stream out-file-name)]
      (let [writer (transit/writer out :json)]
       (transit/write writer model))))

(defn read-model [model-file-name]
  (with-open [in (io/input-stream model-file-name)]
    (let [reader (transit/reader in :json)]
      (transit/read reader))))
