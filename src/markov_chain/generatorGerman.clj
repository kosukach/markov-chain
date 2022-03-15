(ns markov-elear.generatorGerman
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:gen-class))
(def example "And the Golden Grouse And the Pobble who")
(def words (str/split example #" "))
words
(def word-transitions (partition-all 3 1 words))
word-transitions
(merge-with set/union {:a #{1}}  {:a #{2}})
(reduce (fn [r t] (merge-with set/union r
                               (let [[a b c] t]
                                 {[a b] (if c #{c} #{})})))
          {}
          word-transitions)
word-transitions
(defn word-chain [word-transitions]
  (reduce (fn [r t] (merge-with set/union r
                               (let [[a b c d] t]
                                 {[a b c] (if d #{d} #{})})))
          {}
          word-transitions))
word-transitions
(defn text->word-chain [s]
  (let [words (str/split s #"[\s|\n]")
        word-transitions (partition-all 4 1 words)]
    (word-chain word-transitions)))

(defn chain->text [chain]
  (apply str (interpose " " chain)))
  
(defn walk-chain [prefix chain result]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix [(get prefix 1) (get prefix 2) suffix]
            result-with-spaces (chain->text result)
            result-char-count (count result-with-spaces)
            suffix-char-count(inc (count suffix))]
        (println (= (str/last-index-of result-with-spaces ".")
                    (- result-char-count 1)))
        (if (and (>= (+ suffix-char-count result-char-count) 500)
                 (= (str/last-index-of result-with-spaces ".")
                    (- result-char-count 1)))
                 
          result
          (recur new-prefix chain (conj result suffix)))))))
       
(walk-chain ["And" "the"]
            (text->word-chain "And the Golden Grouse And the Pobble who")
            ["And" "the"])
(defn slurp-text [text-files]
  (apply str (map (fn [text-file]
                    (slurp (clojure.java.io/resource text-file)
                           :encoding "Unicode"))
                  text-files)))

(defn convert-cyrilic [unicode-string]
  (let [
        unicode-vector (re-seq #".{1,5}" unicode-string)
        unicode (map #(char(Integer/parseInt %)) unicode-vector)
        text (apply str unicode)
        ]
 text))  

(defn generate-text [start-phrase text-files cyrilic]
  (let[
       prefix (str/split start-phrase #" ")
       text (if (= cyrilic true)
              (convert-cyrilic (slurp-text text-files))
              (slurp-text text-files))
      
       chain (text->word-chain text)
       result-chain (walk-chain prefix chain prefix)
       result-text (chain->text result-chain)
       ]
    
    result-text))

(defn main []
  (println "ushev")
  
  (generate-text "Juden sind die" ["Kampf1.txt"] false)
)
