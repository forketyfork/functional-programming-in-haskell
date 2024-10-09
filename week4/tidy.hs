journeycost :: Float -> Float -> Float
journeycost miles fuelcostperlitre =
    let milespergallon = 35
        litrespergallon = 4.55
        gallons = miles / milespergallon
    in (gallons * litrespergallon * fuelcostperlitre)

squareplusone :: Int -> Int
squareplusone x = xsquared + 1
    where xsquared = x * x

cel2fahr :: Float -> Float
cel2fahr x = (x * scalingfactor) + freezingpoint
    where scalingfactor = 9.0 / 5.0
          freezingpoint = 32
          