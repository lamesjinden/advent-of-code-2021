(ns aoc.day01
  (:require
    [clojure.string :as str]
    [aoc.util :as u]))

(def input-path "resources/input-day01.txt")

(comment
  ; part 1
  (let [lines (-> input-path
                  (slurp)
                  (str/split-lines))
        depths (->> lines
                    (map #(Integer/parseInt %)))
        pairs (->> depths
                   (partition 2 1)
                   (map (fn [[x y]] (> y x)))
                   (filter true?))]
    ; 1226
    (count pairs))

  ; part 2
  (let [lines (-> input-path
                  (slurp)
                  (str/split-lines))
        depths (->> lines
                    (map #(Integer/parseInt %)))
        pairs (->> depths
                   (partition 3 1)
                   (map (fn [[x y z]] (+ x y z)))
                   (partition 2 1)
                   (map (fn [[x y]] (> y x)))
                   (filter true?))]
    ; 1252
    (count pairs))
  )


