module IO
    ( 
        inputStartDots,
        inputDot
    ) where

inputStartDots :: IO [(Double, Double)]
inputStartDots = do
    putStrLn "Ввод первых двух точек (X Y через пробел):"
    str1 <- getLine
    let [x1, y1] = map read $ words str1 :: [Double]
    str2 <- getLine
    let [x2, y2] = map read $ words str2 :: [Double]
    return [(x1, y1), (x2, y2)]
    
inputDot :: IO [(Double, Double)]
inputDot = do
    putStrLn "Ввод точки (X Y через пробел):"
    str1 <- getLine
    if null str1
        then return []
        else do
            let [x, y] = map read $ words str1 :: [Double]
            return [(x, y)]