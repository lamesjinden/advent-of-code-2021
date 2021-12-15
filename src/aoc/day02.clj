(ns aoc.day02
  (:require
    [clojure.string :as str]
    [aoc.util :as u]))

(def input-path "resources/input-day02.txt")

(def ->command-type {"up"      :up
                     "down"    :down
                     "forward" :forward})

(defn parse-command [s]
  (let [tokens (str/split s #" ")
        type (->command-type (first tokens))
        value (Integer/parseInt (second tokens))]
    {:type  type
     :value value}))

(defn run-part1
  ([commands acc]
   (let [{:keys [type value] :as command} (first commands)]
     (if (empty? command)
       acc
       (recur (rest commands)
              (case type
                :up (update acc :depth #(- % value))
                :down (update acc :depth #(+ % value))
                :forward (update acc :horizontal-position #(+ % value)))))))
  ([commands]
   (run-part1 commands {:depth               0
                        :horizontal-position 0})))

(defn run-part2
  ([commands {:keys [aim] :as acc}]
   (let [{:keys [type value] :as command} (first commands)]
     (if (empty? command)
       acc
       (recur (rest commands)
              (case type
                :up (update acc :aim #(- % value))
                :down (update acc :aim #(+ % value))
                :forward (-> acc
                             (update :horizontal-position #(+ % value))
                             (update :depth #(+ % (* aim value)))))))))
  ([commands]
   (run-part2 commands {:depth               0
                        :horizontal-position 0
                        :aim                 0})))

(comment

  ; part 1
  (let [lines (-> input-path
                  (slurp)
                  (str/split-lines))
        commands (->> lines
                      (map parse-command))
        {:keys [depth horizontal-position]} (run-part1 commands)]
    (* depth horizontal-position))

  ; part 2
  (let [lines (-> input-path
                  (slurp)
                  (str/split-lines))
        commands (->> lines
                      (map parse-command))
        {:keys [depth horizontal-position]} (run-part2 commands)]
    (* depth horizontal-position))

  )
