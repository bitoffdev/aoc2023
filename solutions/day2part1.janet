(def grammar
 '{
   :homogeneous-cubes (group (sequence (capture :d+) (some :s) (capture :a+)))
   :heterogeneous-cubes (group (sequence (any (sequence :homogeneous-cubes "," (some :s))) :homogeneous-cubes))
   :subsets (group (sequence (any (sequence :heterogeneous-cubes ";" (some :s))) :heterogeneous-cubes))
   :game (group (sequence "Game " (capture :d+) ":" (some :s) :subsets))
   :games (sequence (any (sequence :game "\n")))
   :main :games})

(defn are-homogeneous-cubes-possible
  [homogeneous-cubes]
  (do
    (def [num-str color] homogeneous-cubes)
    (def num (scan-number num-str))
    (match color
      "red" (<= num 12)
      "green" (<= num 13)
      "blue" (<= num 14))))

(defn are-heterogeneous-cubes-possible
  [heterogeneous-cubes]
  (all are-homogeneous-cubes-possible heterogeneous-cubes))


(defn are-subsets-possible
  [subsets]
  (all are-heterogeneous-cubes-possible subsets))

(defn solve
  [puzzle-input]
  (sum
    (map
      (fn [game]
        (do
          (def [game-num-str subsets] game)
          (def game-num (scan-number game-num-str))
          (match (are-subsets-possible subsets)
            true game-num
            false 0)))
      puzzle-input)))

# Tests
(assert (are-heterogeneous-cubes-possible [["12" "red"] ["14" "blue"]]))
(assert (not (are-heterogeneous-cubes-possible [["13" "red"] ["14" "blue"]])))
(assert (not (are-heterogeneous-cubes-possible [["12" "red"] ["15" "blue"]])))

# (def puzzle-input
# (peg/match grammar `
# Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
# Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
# Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
# Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
# Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
# `))
# (solve puzzle-input)

# (pp 
#   (peg/match grammar
#     (buffer/slice (file/read stdin :all))))

(do
  (def puzzle-input (peg/match grammar (file/read stdin :all)))
  # (pp puzzle-input)
  (print (solve puzzle-input)))
