(ns semi-supervised-multinomial-naive-bayes.classifier
  (:require [clojure.test :refer :all]
            [classifier :refer [word-prob word|class uword-prob classify]]))

(def class1 {:dict {"word1" 4, "word2" 10} :class "+" :doc-count 1 :token 14 :type 2})
(def class2 {:dict {"word1" 5, "word2" 2 "word3" 1} :class "-" :doc-count 1 :token 8 :type 3})
(def unlabeled {:dict {"word1" 100, "word2" 120, "word3" 5} :token 225 :type 3})

(deftest classifier-test
  (testing "word-prob"
    (is (= (/ 505 35112) (word-prob "word1" unlabeled [class1 class2]))))
  (testing "word|class"
    (is (= (/ 5 11) (word|class "word1" [class1 class2]))))
  (testing "uword-prob"
    (is (= (/ 101 228) (uword-prob "word1" unlabeled))))
  (testing "classify"
    (let [result (classify ["word1"] unlabeled [class1 class2])]
      (is (= "-" (:class result)))
      (is (not (nil? (:score result)))))))
