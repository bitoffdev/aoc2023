# Given an array of digits, return the number that concatenates the digits.
#
# For example, the array [1 2] will produce the value 12.
(defn combineDigits [digits] (do (def numDigits (length digits)) (sum (map (fn [i] (* (get digits i) (math/pow 10 (- numDigits i 1)))) (range numDigits)))))

# Return the first and last items of the given array
(defn firstAndLast [arr] [(get arr 0) (get arr (- (length arr) 1))])

# Given a string, produce a list of the digits
(defn extractDigits [line] (map (fn [asciiValue] (- asciiValue 48)) (filter (fn [asciiValue] (<= 48 asciiValue 57)) line)))


# Read all lines from standard input and return them as a list.
(defn readAllLines
  []
  (do
    (def arr @[])
    (var line (file/read stdin :line))
    (while (not= line nil) (array/push arr line) (set line (file/read stdin :line)))
    arr))

# solve
(do 
  (def answer (sum (map (fn [line] (combineDigits (firstAndLast (extractDigits line)))) (readAllLines))))
  (print answer))
