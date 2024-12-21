{-# LANGUAGE GADTs #-}

module EDSL (
    State(..),
    Transition(..),
    FSM(..),
    state,
    transition,
    fsm,
    impossible,
    runFSM,
    showFSM
) where

import Data.List (intercalate)

newtype State = State String deriving (Eq, Show)

data Transition where
    Transition :: State -> String -> State -> Transition
    Impossible :: State -> String -> State -> Transition

instance Show Transition where
    show (Transition (State s1) event (State s2)) = s1 ++ " --" ++ event ++ "--> " ++ s2
    show (Impossible (State s1) event (State s2)) = s1 ++ " -/ " ++ event ++ " /-> " ++ s2

data FSM = FSM {
    states :: [State],
    transitions :: [Transition],
    initialState :: State
} deriving (Show)

state :: String -> State
state = State

transition :: State -> String -> State -> Transition
transition = Transition

impossible :: State -> String -> State -> Transition
impossible = Impossible

fsm :: [State] -> [Transition] -> State -> FSM
fsm = FSM

runFSM :: FSM -> [String] -> State
runFSM (FSM _ trans initState) = foldl applyEvent initState
  where
    applyEvent currentState event = case filter matchTransition trans of
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