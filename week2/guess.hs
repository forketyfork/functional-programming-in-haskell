-- guess-the-number implementation
import System.Random

turn :: Int -> Int -> Int -> IO()
turn number tries input = do
    if tries == 0
        then putStrLn "You Lose!"
        else if input == number
            then putStrLn "You Win!"
            else mkguess number tries

mkguess :: Int -> Int -> IO()
mkguess number tries = do
    putStrLn ("Tries left: " ++ replicate tries '*')
    inputStr <- getLine
    let input = read inputStr :: Int
    let tries' = if input == number then tries else tries - 1
    putStrLn $ compareToString $ compare number input
    turn number tries' input

compareToString :: Ordering -> String
compareToString LT = "Less..."
compareToString GT = "More..."
compareToString EQ = "Correct!"

guess :: IO()
guess = do
    number <- randomRIO (1, 10)
    tries <- randomRIO (3, 5)
    turn number tries (-1)
