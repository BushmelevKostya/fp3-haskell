# Лабораторная работа 4

---

  * Студент: `Бушмелев Константин Алексеевич`
  * Группа: `P3318`
  * ИСУ: `367950`

---

## Ключевые элементы реализации

### `EDSL.hs`

```haskell
data State = State String deriving (Eq, Show)

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
        matchTransition (Impossible _ _ _) = False


showFSM :: FSM -> String
showFSM (FSM states transitions initialState) =
    "States:\n" ++ intercalate "\n" (map show states) ++
    "\n\nTransitions:\n" ++ intercalate "\n" (map show transitions) ++
    "\n\nInitial State:\n" ++ show initialState
```
### `FSMRunner.hs`
```haskell
runFSM :: FSM -> [String] -> State
runFSM (FSM _ transitions initialState) events = foldl applyEvent initialState events
  where
    applyEvent currentState event = case filter matchTransition transitions of
        (Transition _ _ nextState : _) -> nextState
        _ -> currentState
      where
        matchTransition (Transition s1 e s2) = s1 == currentState && e == event
        matchTransition (Impossible _ _ _) = False

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
```
### `Lift.hs`
```haskell
floor1, floor2, floor3, floor4, floor5 :: State
floor1 = state "1st Floor"
floor2 = state "2nd Floor"
floor3 = state "3rd Floor"
floor4 = state "4th Floor"
floor5 = state "5th Floor"

floor1WithPassengers, floor2WithPassengers, floor3WithPassengers, floor4WithPassengers, floor5WithPassengers :: State
floor1WithPassengers = state "1st Floor with Passengers"
floor2WithPassengers = state "2nd Floor with Passengers"
floor3WithPassengers = state "3rd Floor with Passengers"
floor4WithPassengers = state "4th Floor with Passengers"
floor5WithPassengers = state "5th Floor with Passengers"

liftFSM :: FSM
liftFSM = fsm
    [ floor1, floor2, floor3, floor4, floor5
    , floor1WithPassengers, floor2WithPassengers, floor3WithPassengers, floor4WithPassengers, floor5WithPassengers
    ]
    --можно вызвать пустой лифт на любой этаж, кроме текущего
    [ transition floor1 "call 2nd Floor" floor2
    , transition floor1 "call 3rd Floor" floor3
    , transition floor1 "call 4th Floor" floor4
    , transition floor1 "call 5th Floor" floor5
    , transition floor2 "call 1st Floor" floor1
    , transition floor2 "call 3rd Floor" floor3
    , transition floor2 "call 4th Floor" floor4
    , transition floor2 "call 5th Floor" floor5
    , transition floor3 "call 1st Floor" floor1
    , transition floor3 "call 2nd Floor" floor2
    , transition floor3 "call 4th Floor" floor4
    , transition floor3 "call 5th Floor" floor5
    , transition floor4 "call 1st Floor" floor1
    , transition floor4 "call 2nd Floor" floor2
    , transition floor4 "call 3rd Floor" floor3
    , transition floor4 "call 5th Floor" floor5
    , transition floor5 "call 1st Floor" floor1
    , transition floor5 "call 2nd Floor" floor2
    , transition floor5 "call 3rd Floor" floor3
    , transition floor5 "call 4th Floor" floor4
    --можно выпустить пассажиров на первом этаже
    , transition floor1WithPassengers "deliver passengers" floor1
    --можно доставить пассажиров на этажи ниже
    , transition floor2WithPassengers "call 1st Floor" floor1WithPassengers
    , transition floor3WithPassengers "call 2nd Floor" floor2WithPassengers
    , transition floor3WithPassengers "call 1st Floor" floor1WithPassengers
    , transition floor4WithPassengers "call 3rd Floor" floor3WithPassengers
    , transition floor4WithPassengers "call 2nd Floor" floor2WithPassengers
    , transition floor4WithPassengers "call 1st Floor" floor1WithPassengers
    , transition floor5WithPassengers "call 4th Floor" floor4WithPassengers
    , transition floor5WithPassengers "call 3rd Floor" floor3WithPassengers
    , transition floor5WithPassengers "call 2nd Floor" floor2WithPassengers
    , transition floor5WithPassengers "call 1st Floor" floor1WithPassengers
    --нельзя вызвать лифт на тот же этаж
    , impossible floor1 "call 1st Floor" floor1
    , impossible floor2 "call 2nd Floor" floor2
    , impossible floor3 "call 3rd Floor" floor3
    , impossible floor4 "call 4th Floor" floor4
    , impossible floor5 "call 5th Floor" floor5
    --нельзя вызвать лифт с пассажирами на этажи выше
    , impossible floor1WithPassengers "call 1st Floor" floor1WithPassengers
    , impossible floor2WithPassengers "call 2nd Floor" floor2WithPassengers
    , impossible floor3WithPassengers "call 3rd Floor" floor3WithPassengers
    , impossible floor4WithPassengers "call 4th Floor" floor4WithPassengers
    , impossible floor5WithPassengers "call 5th Floor" floor5WithPassengers
    , impossible floor2WithPassengers "call 3nd Floor" floor3WithPassengers
    , impossible floor2WithPassengers "call 4rd Floor" floor4WithPassengers
    , impossible floor2WithPassengers "call 5th Floor" floor5WithPassengers
    , impossible floor3WithPassengers "call 4th Floor" floor4WithPassengers
    , impossible floor3WithPassengers "call 5th Floor" floor5WithPassengers
    , impossible floor4WithPassengers "call 5th Floor" floor5WithPassengers
    --можно посадить пассажиров
    , transition floor1 "board passengers" floor1WithPassengers
    , transition floor2 "board passengers" floor2WithPassengers
    , transition floor3 "board passengers" floor3WithPassengers
    , transition floor4 "board passengers" floor4WithPassengers
    , transition floor5 "board passengers" floor5WithPassengers
    ]
    floor1
```
### `LibTests.hs`
```haskell
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
```

## Ввод/вывод программы
### Ввод
 -Список событий для лифта.
### Вывод
```
Lift FSM:
States:
State "1st Floor"
State "2nd Floor"
State "3rd Floor"
State "4th Floor"
State "5th Floor"
State "1st Floor with Passengers"
State "2nd Floor with Passengers"
State "3rd Floor with Passengers"
State "4th Floor with Passengers"
State "5th Floor with Passengers"

Transitions:
1st Floor --call 2nd Floor--> 2nd Floor
1st Floor --call 3rd Floor--> 3rd Floor
1st Floor --call 4th Floor--> 4th Floor
1st Floor --call 5th Floor--> 5th Floor
2nd Floor --call 1st Floor--> 1st Floor
2nd Floor --call 3rd Floor--> 3rd Floor
2nd Floor --call 4th Floor--> 4th Floor
2nd Floor --call 5th Floor--> 5th Floor
3rd Floor --call 1st Floor--> 1st Floor
3rd Floor --call 2nd Floor--> 2nd Floor
3rd Floor --call 4th Floor--> 4th Floor
3rd Floor --call 5th Floor--> 5th Floor
4th Floor --call 1st Floor--> 1st Floor
4th Floor --call 2nd Floor--> 2nd Floor
4th Floor --call 3rd Floor--> 3rd Floor
4th Floor --call 5th Floor--> 5th Floor
5th Floor --call 1st Floor--> 1st Floor
5th Floor --call 2nd Floor--> 2nd Floor
5th Floor --call 3rd Floor--> 3rd Floor
5th Floor --call 4th Floor--> 4th Floor
1st Floor with Passengers --deliver passengers--> 1st Floor
2nd Floor with Passengers --call 1st Floor--> 1st Floor with Passengers
3rd Floor with Passengers --call 2nd Floor--> 2nd Floor with Passengers
3rd Floor with Passengers --call 1st Floor--> 1st Floor with Passengers
4th Floor with Passengers --call 3rd Floor--> 3rd Floor with Passengers
4th Floor with Passengers --call 2nd Floor--> 2nd Floor with Passengers
4th Floor with Passengers --call 1st Floor--> 1st Floor with Passengers
5th Floor with Passengers --call 4th Floor--> 4th Floor with Passengers
5th Floor with Passengers --call 3rd Floor--> 3rd Floor with Passengers
5th Floor with Passengers --call 2nd Floor--> 2nd Floor with Passengers
5th Floor with Passengers --call 1st Floor--> 1st Floor with Passengers
1st Floor -/ call 1st Floor /-> 1st Floor
2nd Floor -/ call 2nd Floor /-> 2nd Floor
3rd Floor -/ call 3rd Floor /-> 3rd Floor
4th Floor -/ call 4th Floor /-> 4th Floor
5th Floor -/ call 5th Floor /-> 5th Floor
1st Floor with Passengers -/ call 1st Floor /-> 1st Floor with Passengers
2nd Floor with Passengers -/ call 2nd Floor /-> 2nd Floor with Passengers
3rd Floor with Passengers -/ call 3rd Floor /-> 3rd Floor with Passengers
4th Floor with Passengers -/ call 4th Floor /-> 4th Floor with Passengers
5th Floor with Passengers -/ call 5th Floor /-> 5th Floor with Passengers
2nd Floor with Passengers -/ call 3nd Floor /-> 3rd Floor with Passengers
2nd Floor with Passengers -/ call 4rd Floor /-> 4th Floor with Passengers
2nd Floor with Passengers -/ call 5th Floor /-> 5th Floor with Passengers
3rd Floor with Passengers -/ call 4th Floor /-> 4th Floor with Passengers
3rd Floor with Passengers -/ call 5th Floor /-> 5th Floor with Passengers
4th Floor with Passengers -/ call 5th Floor /-> 5th Floor with Passengers
1st Floor --board passengers--> 1st Floor with Passengers
2nd Floor --board passengers--> 2nd Floor with Passengers
3rd Floor --board passengers--> 3rd Floor with Passengers
4th Floor --board passengers--> 4th Floor with Passengers
5th Floor --board passengers--> 5th Floor with Passengers

Initial State:
State "1st Floor"

Running events:
Event: call 2nd Floor, State: State "2nd Floor"
Event: call 3rd Floor, State: State "3rd Floor"
Event: call 1st Floor, State: State "1st Floor"
Event: call 5th Floor, State: State "5th Floor"
Event: board passengers, State: State "5th Floor with Passengers"
Event: call 4th Floor, State: State "4th Floor with Passengers"
Event: call 3rd Floor, State: State "3rd Floor with Passengers"
Event: call 2nd Floor, State: State "2nd Floor with Passengers"
Event: call 1st Floor, State: State "1st Floor with Passengers"
Event: deliver passengers, State: State "1st Floor"
Final State: State "1st Floor"

DOT description saved to fsm.dot
```
