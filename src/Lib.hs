module Lib
    ( interpolate,
      linearInterpolation,
      quadraticInterpolation,
      lagrangeInterpolation
    ) where

roundTo :: Int -> Double -> Double
roundTo n f = fromInteger (round $ f * (10^n)) / (10.0^^n)

linearInterpolation :: [(Double, Double)] -> Double -> [(Double, Double)]
linearInterpolation dots step = 
    let 
        linearDotsLength = 2

        (x2, y2) = last dots
        (x1, y1) = last $ init dots
        k = (y2 - y1) / (x2 - x1)
        b = y1 - k * x1
        generatePoints x
            | x > x2 = []
            | otherwise = (roundTo 2 x, roundTo 2 (k * x + b)) : generatePoints (x + step)
    in
        if length dots >= linearDotsLength
        then generatePoints x1
        else []

quadraticInterpolation :: [(Double, Double)] -> Double -> [(Double, Double)]
quadraticInterpolation dots step =
    let 
        quadraticDotsLength = 3

        (x3, y3) = last dots
        (x2, y2) = last $ init dots
        (x1, y1) = last $ init $ init dots
        a = ((y3 - y1) / (x3 - x1) - (y2 - y1) / (x2 - x1)) / (x3 - x2)
        b = (y2 - y1) / (x2 - x1) - a * (x1 + x2)
        c = y1 - a * x1 * x1 - b * x1
        generatePoints x
            | x > x3 = []
            | otherwise = (roundTo 2 x, roundTo 2 (a * x * x + b * x + c)) : generatePoints (x + step)
    in 
        if length dots >= quadraticDotsLength
        then generatePoints x1
        else []

lagrangeInterpolation :: [(Double, Double)] -> Double -> [(Double, Double)]
lagrangeInterpolation dots step =
    let 
        lagranzhDotsLength = 6

        (x6, y6) = last dots
        (x5, y5) = last $ init dots
        (x4, y4) = last $ init $ init dots
        (x3, y3) = last $ init $ init $ init dots
        (x2, y2) = last $ init $ init $ init $ init dots
        (x1, y1) = last $ init $ init $ init $ init $ init dots
        l1 x = (x - x2) * (x - x3) * (x - x4) * (x - x5) * (x - x6) / ((x1 - x2) * (x1 - x3) * (x1 - x4) * (x1 - x5) * (x1 - x6))
        l2 x = (x - x1) * (x - x3) * (x - x4) * (x - x5) * (x - x6) / ((x2 - x1) * (x2 - x3) * (x2 - x4) * (x2 - x5) * (x2 - x6))
        l3 x = (x - x1) * (x - x2) * (x - x4) * (x - x5) * (x - x6) / ((x3 - x1) * (x3 - x2) * (x3 - x4) * (x3 - x5) * (x3 - x6))
        l4 x = (x - x1) * (x - x2) * (x - x3) * (x - x5) * (x - x6) / ((x4 - x1) * (x4 - x2) * (x4 - x3) * (x4 - x5) * (x4 - x6))
        l5 x = (x - x1) * (x - x2) * (x - x3) * (x - x4) * (x - x6) / ((x5 - x1) * (x5 - x2) * (x5 - x3) * (x5 - x4) * (x5 - x6))
        l6 x = (x - x1) * (x - x2) * (x - x3) * (x - x4) * (x - x5) / ((x6 - x1) * (x6 - x2) * (x6 - x3) * (x6 - x4) * (x6 - x5))
        generatePoints x
            | x > x6 = []
            | otherwise = (roundTo 2 x, roundTo 2 (y1 * l1 x + y2 * l2 x + y3 * l3 x + y4 * l4 x + y5 * l5 x + y6 * l6 x)) : generatePoints (x + step)

    in
        if length dots >= lagranzhDotsLength
        then generatePoints x1
        else []

interpolate :: [(Double, Double)] -> Double -> [[(Double, Double)]]
interpolate dots step =
    let 
        linearDotsLength = 2
        quadraticDotsLength = 3
        lagrangeDotsLength = 6
    in
        let 
            linearResult =
                if length dots >= linearDotsLength
                then linearInterpolation dots step
                else []
            quadraticResult =
                if length dots >= quadraticDotsLength
                then quadraticInterpolation dots step
                else []
            lagranzhResult =
                if length dots >= lagrangeDotsLength
                then lagrangeInterpolation dots step
                else []
        in
            [linearResult, quadraticResult, lagranzhResult]