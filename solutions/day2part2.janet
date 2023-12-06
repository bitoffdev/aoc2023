(def grammar
 '{
   :homogeneous-cubes (group (sequence (capture :d+) (some :s) (capture :a+)))
   :heterogeneous-cubes (group (sequence (any (sequence :homogeneous-cubes "," (some :s))) :homogeneous-cubes))
   :subsets (group (sequence (any (sequence :heterogeneous-cubes ";" (some :s))) :heterogeneous-cubes))
   :game (group (sequence "Game " (capture :d+) ":" (some :s) :subsets))
   :games (sequence (any (sequence :game "\n")))
   :main :games})

(defn homogeneous-cubes-max
  [homogeneous-cubes]
  (do
    (def [num-str color] homogeneous-cubes)
    (def num (scan-number num-str))
    (match color
      "red" {:red num :green 0 :blue 0}
      "green" {:red 0 :green num :blue 0}
      "blue" {:red 0 :green 0 :blue num})))

(defn heterogeneous-cubes-max
  [heterogeneous-cubes]
  (do
    (def maxes (map homogeneous-cubes-max heterogeneous-cubes))
    {:red (max-of (map (fn [x] (get x :red)) maxes))
     :green (max-of (map (fn [x] (get x :green)) maxes))
     :blue (max-of (map (fn [x] (get x :blue)) maxes))}))

(defn subset-max
  [subset]
  (do
    (def maxes (map heterogeneous-cubes-max subset))
    {:red (max-of (map (fn [x] (get x :red)) maxes))
     :green (max-of (map (fn [x] (get x :green)) maxes))
     :blue (max-of (map (fn [x] (get x :blue)) maxes))}))

(defn subset-score
  [subset]
  (do
    (def max-per-color (subset-max subset))
    (*
      (get max-per-color :red)
      (get max-per-color :green)
      (get max-per-color :blue))))

(defn solve
  [puzzle-input]
  (sum
    (map
      (fn [game]
        (do
          (def [_ subset] game)
	  (subset-score subset)))
      puzzle-input)))

# Tests
(assert (= (subset-max [[["3" "red"] ["5" "blue"] ["7" "red"]]]) {:blue 5 :green 0 :red 7}))

(do
  (def puzzle-input (peg/match grammar (file/read stdin :all)))
  (print (solve puzzle-input)))
