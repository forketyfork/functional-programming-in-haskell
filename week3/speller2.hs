speller :: [[Char]] -> [Char]
-- corner case with an empty list
speller [] = ""
-- corner case with a list of a single element
speller [x] = spell x
-- base case with at least 2 elements, to write "and" before the last one
speller [x, y] = spell x ++ ", and " ++ spell y
-- induction case with more than 2 elements
speller (x : y : xs) = spell x ++ ", " ++ speller (y : xs)

-- helper function to convert a word into a spelling, like "a is for apple"
spell :: [Char] -> [Char]
spell (x:xs) = x : " is for " ++ x : xs
