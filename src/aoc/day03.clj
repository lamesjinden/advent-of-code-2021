(ns aoc.day03
  (:require
    [clojure.string :as str]
    [aoc.util :as u]
    [clojure.core.matrix :as m]))

(def input-path "resources/input-day03.txt")

(defn eval-bitness [bits common-zeros common-ones]
  (let [counts (frequencies bits)
        zeros (get counts \0 0)
        ones (get counts \1 0)]
    (if (> zeros ones)
      common-zeros
      common-ones)))

(defn ->gamma-bit [bits]
  (eval-bitness bits "0" "1"))

(defn ->epsilon-bit [bits]
  (eval-bitness bits "1" "0"))

(defn rating-filter [commonality-fn m column]
  (let [common-value (commonality-fn m column)
        filtered (->> m
                      (m/rows)
                      (filter #(= common-value (get % column))))]
    (m/matrix filtered)))

(defn compute-rating [commonality-fn m]
  (loop [m m
         column 0]
    (if (= 1 (m/row-count m))
      (->> m
           (first)
           (str/join)
           (u/bin-str->int))
      (let [column-filtered (rating-filter commonality-fn m column)]
        (recur column-filtered (inc column))))))

(defn commonest [m column common-zeros common-ones common-equal]
  (let [counts (frequencies (m/get-column m column))
        zeros (get counts \0 0)
        ones (get counts \1 0)
        most-common (cond
                      (> zeros ones) common-zeros
                      (< zeros ones) common-ones
                      :else common-equal)]
    most-common))

(defn most-common [m column]
  (commonest m column \0 \1 \1))

(defn compute-o2-rating [m]
  (compute-rating most-common m))

(defn least-common [m column]
  (commonest m column \1 \0 \0))

(defn compute-co2-rating [m]
  (compute-rating least-common m))

(comment

  ; part 1 - 775304
  (let [lines (-> input-path
                  (slurp)
                  (str/split-lines))
        data (->> lines
                  (mapv vec)
                  (u/rotate-cw))
        gamma-bits (->> data
                        (map ->gamma-bit)
                        (str/join))
        gamma (u/bin-str->int gamma-bits)
        epsilon-bit (->> data
                         (map ->epsilon-bit)
                         (str/join))
        epsilon (u/bin-str->int epsilon-bit)]
    (* gamma epsilon))

  ; part 2 - 1370737
  (let [lines (-> input-path
                  (slurp)
                  (str/split-lines))
        data (->> lines
                  (mapv vec)
                  (m/matrix))
        o2-rating (compute-o2-rating data)
        co2-rating (compute-co2-rating data)
        life-support-rating (* o2-rating co2-rating)]
    life-support-rating)

  )
