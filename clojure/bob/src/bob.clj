(ns bob
  (:require [clojure.string :refer [blank?]]))

(defn yelling? [phrase]
  (let [letters (filter #(Character/isLetter %) phrase)]
    (if (seq letters)
      (every? #(Character/isUpperCase %) letters)
      false)))

(defn question? [phrase]
  (= (last phrase) \?))

(defn response-for [phrase]
  (cond
    (blank? phrase) "Fine. Be that way!"
    (yelling? phrase) "Whoa, chill out!"
    (question? phrase) "Sure."
    :else "Whatever."))