import Test.HUnit
import Lift (liftFSM)
import FSMRunner (runFSM)
import qualified EDSL as EDSL

runLiftFSM :: [String] -> EDSL.State
runLiftFSM events = runFSM liftFSM events

--перемещение лифта
test1 :: Test
test1 = TestCase (assertEqual "for events [\"call 2nd Floor\", \"call 3rd Floor\"]"
                  (EDSL.State "3rd Floor")
                  (runLiftFSM ["call 2nd Floor", "call 3rd Floor"]))
--забор и привоз пассажирова
test2 :: Test
test2 = TestCase (assertEqual "for events [\"call 5th Floor\", \"board passengers\", \"call 1st Floor\", \"deliver passengers\"]"
                  (EDSL.State "1st Floor")
                  (runLiftFSM ["call 5th Floor", "board passengers", "call 1st Floor", "deliver passengers"]))

--невозможность перехода на тот же этаж
test3 :: Test
test3 = TestCase (assertEqual "for events [\"call 3rd Floor\", \"call 3rd Floor\"]"
                  (EDSL.State "3rd Floor")
                  (runLiftFSM ["call 3rd Floor", "call 3rd Floor"]))

--невозможность перехода на этаж выше с пассажирами
test4 :: Test
test4 = TestCase (assertEqual "for events [\"call 5th Floor\", \"board passengers\", \"call 2nd Floor\", \"call 3rd Floor\"]"
                  (EDSL.State "2nd Floor with Passengers")
                  (runLiftFSM ["call 5th Floor", "board passengers", "call 2nd Floor", "call 3rd floor"]))

tests :: Test
tests = TestList [TestLabel "Test1" test1,
                  TestLabel "Test2" test2,
                  TestLabel "Test3" test3,
                  TestLabel "Test4" test4]

main :: IO Counts
main = runTestTT tests