(ns aoc.day04
  (:require
    [clojure.string :as str]
    [aoc.util :as u]
    [clojure.core.matrix :as m]))

(def input-path "resources/input-day04.txt")

(def sample-input "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7")

(defn parse-row [s]
  (as-> s $
        (str/split $ #"\s+")
        (filter #(not= "" %) $)
        (mapv #(Integer/parseInt %) $)))

(defn parse-board [ss]
  (map parse-row ss))

(defn ->board [ss]
  {:grid   (->> ss
                (parse-board)
                (m/matrix))
   :marked #{}})

(defn mark-number [board number]
  (update-in board [:marked] (fn [prev n] (conj prev n)) number))

(defn winner? [{:keys [grid marked] :as _board}]
  (let [evaluator (fn [xs] (every? #(contains? marked %) xs))
        winner (or (some evaluator (m/rows grid))
                   (some evaluator (m/columns grid)))]
    winner))

(defn find-winner [boards]
  (some #(and (winner? %) %) boards))

(defn any-winner? [boards]
  (not (nil? (find-winner boards))))

(defn get-unmarked-numbers [{:keys [grid marked] :as _board}]
  (->> grid
       (mapcat identity)
       (filter #(not (contains? marked %)))))

(defn take-turn [boards selection]
  (let [next-boards (map #(mark-number % selection) boards)]
    (if-let [winner (find-winner next-boards)]
      (reduced {:selection selection
                :winner    winner})
      next-boards)))

(defn take-turn-wins-last [{:keys [boards winners] :as acc} selection]
  (let [next-boards (map #(mark-number % selection) boards)
        [current-winners non-winners] ((juxt (partial filter winner?)
                                             (partial filter (complement winner?)))
                                       next-boards)]
    (if (seq current-winners)
      {:boards         non-winners
       :winners        (apply conj winners current-winners)
       :last-selection selection}
      (assoc acc :boards next-boards))))

(comment

  (let [input (u/group-by-newline (slurp input-path))
        selections (as-> input $
                         (ffirst $)
                         (str/split $ #",")
                         (map #(Integer/parseInt %) $))
        boards (as-> input $
                     (drop 1 $)
                     (map ->board $))
        end-state (reduce take-turn boards selections)
        final-selection (:selection end-state)
        winning-board (:winner end-state)
        winning-unmarked (get-unmarked-numbers winning-board)
        result (* final-selection (reduce + winning-unmarked))]
    result) ; 6592

  (let [input (u/group-by-newline (slurp input-path))
        selections (as-> input $
                         (ffirst $)
                         (str/split $ #",")
                         (map #(Integer/parseInt %) $))
        boards (as-> input $
                     (drop 1 $)
                     (map ->board $))
        {:keys [last-selection winners]} (reduce take-turn-wins-last
                                                 {:boards         boards
                                                  :winners        []
                                                  :last-selection 0}
                                                 selections)
        winning-unmarked (-> winners
                             (last)
                             (get-unmarked-numbers))
        result (* last-selection (reduce + winning-unmarked))]
    result) ; 31755

  )
