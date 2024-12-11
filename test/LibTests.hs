import Test.HUnit
import Lib (interpolate, linearInterpolation, quadraticInterpolation, lagrangeInterpolation)

testLinearInterpolation :: Test
testLinearInterpolation = TestList [
    TestCase $ assertEqual "Linear Interpolation: Two points" 
        [(1.0,2.0),(1.6,3.2),(2.2,4.4),(2.8,5.6)] 
        (linearInterpolation [(1.0, 2.0), (3.0, 6.0)] 0.6)
    ]

testQuadraticInterpolation :: Test
testQuadraticInterpolation = TestList [
    TestCase $ assertEqual "Quadratic Interpolation: Three points" 
        [(1.0,1.0),(1.6,2.92),(2.2,4.48),(2.8,5.68)] 
        (quadraticInterpolation [(1.0, 1.0), (2.0, 4.0), (3.0, 6.0)] 0.6)
    ]

testLagrangeInterpolation :: Test
testLagrangeInterpolation = TestList [
    TestCase $ assertEqual "Lagrange Interpolation: Six points" 
        [(1.0,1.0),(1.6,3.73),(2.2,10.78),(2.8,22.05),(3.4,39.13),(4.0,64.0),(4.6,97.82),(5.2,139.66),(5.8,185.31)] 
        (lagrangeInterpolation [(1.0, 1.0), (2.0, 8.0), (3.0, 27.0), (4.0, 64.0), (5.0, 125.0), (6.0, 200.0)] 0.6)
    ]

testInterpolate :: Test
testInterpolate = TestList [
    TestCase $ assertEqual "Interpolate: Two points (Linear only)" 
        [[(1.0,2.0),(1.6,3.2),(2.2,4.4),(2.8,5.6)],[],[]] 
        (interpolate [(1.0, 2.0), (3.0, 6.0)] 0.6),

    TestCase $ assertEqual "Interpolate: Three points (Linear + Quadratic)" 
        [[(2.0,4.0),(2.6,5.2)],[(1.0,1.0),(1.6,2.92),(2.2,4.48),(2.8,5.68)],[]] 
        (interpolate [(1.0, 1.0), (2.0, 4.0), (3.0, 6.0)] 0.6),

    TestCase $ assertEqual "Interpolate: Six points (All)" 
        [[(5.0,125.0),(5.6,170.0)],[(4.0,64.0),(4.6,98.92),(5.2,138.88),(5.8,183.88)],[(1.0,1.0),(1.6,3.73),(2.2,10.78),(2.8,22.05),(3.4,39.13),(4.0,64.0),(4.6,97.82),(5.2,139.66),(5.8,185.31)]] 
        (interpolate [(1.0, 1.0), (2.0, 8.0), (3.0, 27.0), (4.0, 64.0), (5.0, 125.0), (6.0, 200.0)] 0.6)
    ]

tests :: Test
tests = TestList [testLinearInterpolation, testQuadraticInterpolation, testLagrangeInterpolation, testInterpolate]

main :: IO ()
main = do
    runTestTT tests
    return ()