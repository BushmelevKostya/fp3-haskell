module Main (main) where

import Lib (interpolate, linearInterpolation, quadraticInterpolation, lagrangeInterpolation)
import IO (inputDot, inputStartDots)
import System.Environment (getArgs)
import Data.List (sortBy)
import Data.Ord (comparing)

formatResults :: [[(Double, Double)]] -> String
formatResults [linear, quadratic, lagrange] =
    "Линейная: " ++ show linear ++ ",\n" ++
    "Квадратичная: " ++ show quadratic ++ ",\n" ++
    "Лагранж: " ++ show lagrange
formatResults _ = error "Unexpected number of results"

main :: IO ()
main = do
    args <- getArgs
    let 
        step = if not (null args) then read (head args) :: Double else 1.0
        typ = if length args > 1 then read (args !! 1) :: Int else 1
        dots = []
    processDots dots step typ

processDots :: [(Double, Double)] -> Double -> Int -> IO ()
processDots dots step typ = do
    let
        sortedDots = sortBy (comparing fst) dots
        outputType =
            case typ of
                1 -> "Линейная"
                2 -> "Квадратичная"
                3 -> "Лагранж"
                4 -> "Линейная, Квадратичная и Лагранж"
                _ -> error "Unknown interpolation type"
        res = 
            case typ of
                1 -> [linearInterpolation sortedDots step]
                2 -> [quadraticInterpolation sortedDots step]
                3 -> [lagrangeInterpolation sortedDots step]
                4 -> interpolate sortedDots step
                _ -> error "Unknown interpolation type"
        formattedRes = formatResults res
    if null $ head res
        then return()
        else if length res == 3
            then
                putStrLn $ outputType ++ " интерполяция с шагом " ++ show step ++ ":\n" ++ formattedRes
            else do
                putStrLn $ outputType ++ " интерполяция с шагом " ++ show step ++ ":"
                print res
    dot <- inputDot
    processDots (sortedDots ++ dot) step typ