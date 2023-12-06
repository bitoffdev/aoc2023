# Given an array of digits, return the number that concatenates the digits.
#
# For example, the array [1 2] will produce the value 12.
(defn combineDigits [digits] (do (def numDigits (length digits)) (sum (map (fn [i] (* (get digits i) (math/pow 10 (- numDigits i 1)))) (range numDigits)))))

# Read all lines from standard input and return them as a list.
(defn readAllLines
  []
  (do
    (def arr @[])
    (var line (file/read stdin :line))
    (while (not= line nil) (array/push arr line) (set line (file/read stdin :line)))
    arr))

(def wordValues (struct
  "0" 0
  "1" 1
  "2" 2
  "3" 3
  "4" 4
  "5" 5
  "6" 6
  "7" 7
  "8" 8
  "9" 9
  "zero" 0
  "one" 1
  "two" 2
  "three" 3
  "four" 4
  "five" 5
  "six" 6
  "seven" 7
  "eight" 8
  "nine" 9
  "ten" 10))

(defn firstAndLast
  [line]
  (do
    (var firstPos nil)
    (var firstValue nil)
    (var lastPos nil)
    (var lastValue nil)
    (loop [[word value] :pairs wordValues]
      (do
        (def positions (string/find-all word line))
        (loop [pos :in positions]
          (do
            (if (or (= firstPos nil) (< pos firstPos)) (do
              (set firstPos pos)
              (set firstValue value)))
            (if (or (= lastPos nil) (> pos lastPos)) (do
              (set lastPos pos)
              (set lastValue value)))
              ))))
     [firstValue lastValue]))


# solve
(do 
  (def answer (sum (map (fn [line] (combineDigits (firstAndLast line))) (readAllLines))))
  (print answer))
