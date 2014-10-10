(ns classifier
  (use [clojure.contrib.generic.math-functions :only (log)]))

(defn prior-prob
  [[& more-dict]]
     (let [freq1 (:doc-count (first more-dict))
           freq-more (map #(:doc-count %) more-dict)
           numerator (inc freq1)
           denominator (+ (reduce + freq-more)
                          (count more-dict))]
       (/ numerator denominator)))

(defn uword-prob
  [word unlabeled-dict]
  (/ (inc (get (:dict unlabeled-dict) word 0))
     (+ (:type unlabeled-dict) (:token unlabeled-dict))))

(defn word|class
  [word [& more-dict]]
  (let [num1 (get (:dict (first more-dict)) word 0)
        sum (reduce + (map #(get (:dict %) word 0) more-dict))]
    (if (= sum 0)
      0
      (/ (inc num1) (+ sum (count more-dict))))))

(defn joint-prob
  [word udict [& more-dict]]
  (* (word|class word more-dict) (uword-prob word udict)))

(defn word-prob
  [word udict [& more-dict]]
  (/ (joint-prob word udict more-dict)
     (:token (first more-dict))))

(defn score
  [words udict [& more-dict]]
  (let [wprobs (->> (map #(word-prob % udict more-dict) words)
                    (remove #(= 0 %)))]
    (reduce + (map #(log %) (cons (prior-prob more-dict) wprobs)))))

(defn classify
  [words udict [& more-dict]]
  (letfn [(self [n]
            (let [coll (take (count more-dict) (drop n (cycle more-dict)))
                  main-class (:class (first coll))
                  score (score words udict coll)]
              {:class main-class :score score}))]
    (let [coll  (map #(self %) (range (count more-dict)))]
      (if (apply = coll)
        {:class :none :score 0}
      (first (sort-by :score > coll))))))


