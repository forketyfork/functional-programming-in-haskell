absolute :: (Ord a, Num a) => a -> a
absolute x
    | x < 0     = -x
    | otherwise = x

holeScore :: Int -> Int -> String 
holeScore strokes par
    | score < 0 = show (abs score) ++ " under par"
    | strokes == 0 = "level par"
    | otherwise = show score ++ " over par"
    where score = strokes - par

data Pet = Cat | Dog | Fish | Parrot String | Pig

hello :: Pet -> String
hello x =
    case x of
        Cat -> "meow"
        Dog -> "woof"
        Fish -> "bubble"
        Parrot name -> "pretty " ++ name
        _ -> "grunt"

