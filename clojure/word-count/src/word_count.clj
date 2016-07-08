(ns word-count
  (require [clojure.string :refer [split lower-case]]))

(defn strip-special-chars [word]
  (apply str (filter #(Character/isLetterOrDigit %) word)))

(defn word-count [text]
  (->> (split text #" ")
       (map (comp strip-special-chars lower-case))
       (remove empty?)
       (frequencies)))