(ns rna-transcription)

(defn rna-complement [base]
  {:pre [(#{\G \C \T \A} base)]}
  (case base
    \G \C
    \C \G
    \T \A
    \A \U))

(defn to-rna [bases]
  (apply str (map rna-complement bases)))