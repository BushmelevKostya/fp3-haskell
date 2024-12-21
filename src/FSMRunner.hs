module FSMRunner (
    runFSM,
    showFSM,
    toDot
) where

import EDSL (FSM(..), State(..), Transition(..))
import Data.List (intercalate)

runFSM :: FSM -> [String] -> State
runFSM (FSM _ transitions initialState) = foldl applyEvent initialState
  where
    applyEvent currentState event = case filter matchTransition transitions of
        (Transition _ _ nextState : _) -> nextState
        _ -> currentState
      where
        matchTransition (Transition s1 e s2) = s1 == currentState && e == event
        matchTransition Impossible {} = False

showFSM :: FSM -> String
showFSM (FSM states transitions initialState) =
    "States:\n" ++ intercalate "\n" (map show states) ++
    "\n\nTransitions:\n" ++ intercalate "\n" (map show transitions) ++
    "\n\nInitial State:\n" ++ show initialState

toDot :: FSM -> String
toDot (FSM states transitions initialState) =
    "digraph FSM {\n" ++
    "  rankdir=LR;\n" ++
    "  node [shape = circle];\n" ++
    "  " ++ show initialState ++ " [shape = doublecircle];\n" ++
    concatMap showTransition transitions ++
    "}\n"
  where
    showTransition (Transition (State s1) event (State s2)) =
        "  \"" ++ s1 ++ "\" -> \"" ++ s2 ++ "\" [label = \"" ++ event ++ "\"];\n"
    showTransition (Impossible (State s1) event (State s2)) =
        "  \"" ++ s1 ++ "\" -> \"" ++ s2 ++ "\" [label = \"" ++ event ++ "\", style = \"dashed\"];\n"