import Lift (liftFSM)
import FSMRunner (runFSM, showFSM, toDot)
import EDSL (FSM(..), State(..), Transition(..))
import System.IO (writeFile)

runAndShowFSM :: FSM -> [String] -> IO ()
runAndShowFSM fsm events = go fsm events (initialState fsm)
  where
    initialState (FSM _ _ initial) = initial
    go _ [] currentState = putStrLn $ "Final State: " ++ show currentState
    go fsm (e:es) currentState = do
        let nextState = runFSM (FSM (states fsm) (transitions fsm) currentState) [e]
        putStrLn $ "Event: " ++ e ++ ", State: " ++ show nextState
        go fsm es nextState

main :: IO ()
main = do
    putStrLn "Lift FSM:"
    putStrLn $ showFSM liftFSM

    let events = ["call 2nd Floor", "call 3rd Floor", "call 1st Floor", "call 5th Floor", "board passengers", "call 4th Floor", "call 3rd Floor", "call 2nd Floor", "call 1st Floor", "deliver passengers"]
    putStrLn "\nRunning events:"
    runAndShowFSM liftFSM events

    let dotDescription = toDot liftFSM

    writeFile "fsm.dot" dotDescription
    putStrLn "\nDOT description saved to fsm.dot"