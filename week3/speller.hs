import Data.List

-- the speller function does the following:
-- * converts words to spellings
-- * prepends "and" to the last one (if there's more than one word in the list)
-- * joins the array into a string with ", " string in between the words
speller :: [[Char]] -> [Char]
speller = joinWithCommas . prependAnd . spellings

-- convert a word to a spelling according to the first letter
-- e.g. "banana" -> "b is for banana"
spellword :: [Char] -> [Char]
spellword lst = [head lst] ++ " is for " ++ lst

-- convert a list of strings to a list of spellings, e.g.
-- ["apple", "banana"] -> ["a is for apple", "b is for banana"]
spellings :: [[Char]] -> [[Char]]
spellings = map spellword

-- join strings into a single string with ", " between them
joinWithCommas :: [[Char]] -> [Char]
joinWithCommas = intercalate ", "

-- prepend "and " to the last string in the list of 2 or more elements
prependAnd :: [[Char]] -> [[Char]]
prependAnd [] = []
prependAnd [x] = [x]
prependAnd xs = foldr (\elt acc -> 
    if null acc 
        then ["and " ++ elt] 
        else elt : acc) 
        [] xs
