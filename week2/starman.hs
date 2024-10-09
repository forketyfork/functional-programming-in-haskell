import System.Random

dict = ["proactively", "leverage", "synergies"]

check :: String -> String -> Char -> (Bool,String)
check word display c
    = (c `elem` word, [if x == c
        then c
        else y | (x, y) <- zip word display])

turn :: String -> String -> Int -> IO()
turn word display n =
    do if n == 0
        then putStrLn "you lose"
        else if word == display
            then putStrLn "you win!"
            else mkguess word display n

mkguess :: String -> String -> Int -> IO()
mkguess word display n =
    do putStrLn (display ++ " " ++ replicate n '*')
       putStr "  Enter your guess: "
       q <- getLine
       let (correct, display') = check word display (head q)
       let n' = if correct then n else n - 1
       turn word display' n'

starman :: IO()
starman = do
    idx <- randomRIO (0, length dict)
    let word = dict !! idx
    n <- randomRIO (3, 10)
    turn word ['-' | x <- word] n
