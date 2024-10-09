import Data.Char

shouldcipher :: Char -> Bool
shouldcipher c = isAsciiLower c || isAsciiUpper c

cipherchar :: Int -> Char -> Char
cipherchar shift c
  | shouldcipher c  = cipherletter shift c
  | otherwise       = c

cipherletter :: Int -> Char -> Char
cipherletter shift c = chr . adjustbase c . (+shift) . ord $ c

adjustbase :: Char -> Int -> Int
adjustbase c i 
    | isLower c = ((i - ord 'a') `mod` 26) + ord 'a'
    | isUpper c = ((i - ord 'A') `mod` 26) + ord 'A'
    | otherwise = i

cipher :: Int -> String -> String
cipher shift = map (cipherchar shift)

decipher :: Int -> String -> String
decipher shift = map (cipherchar (negate shift))