module Lift (
    liftFSM,
    showFSM,
    runFSM
) where

import EDSL (State, FSM, state, fsm, transition, impossible, showFSM, runFSM)

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