import Debug
import List exposing (..)
import Maybe exposing (Maybe)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)


import Lift



type alias Lift : {from: Int, to: Int, pax: List Passenger}

isStopped lift = lift.from == lift.to
isEmpty lift = isEmpty lift.pax


type CEvent : Call Int | Arrive Int Lift | Leave Int Lift

type alias CState : List Lift

num_lifts = 2
lift_cap = 2

init_controller : CState
init_controller = repeat num_lifts { from = 0, to = 0, pax = [] }

step_controller : CEvent -> CState -> CState
step_controller   input     lifts  = case input of
  Arrive floor_num lift ->
    -- schedule leave
  Leave lift ->
    -- schedule arrive
  Call to ->
    findIndex (\lift -> isEmpty l && isStopped l && l.to != to) `andThen` \i ->
      (flip indexedMap) lifts \j lift ->
        if i != j then lift else { lift | to = to}
          -- schedule arrive

controller : Automaton Command Lift
controller = state init_controller step_controller











initial_input : Maybe Int
initial_input = Nothing

initial_state = {
  floors: [
    [1, 2, 3],
    [0, 2, 3],
    [0, 1, 3],
    [0, 1, 2]
  ],
  lifts :
}


num_floors = length initial_state.floors
num_lifts  = length initial_state.lifts



exchange : Int       -> (Floor, Lift) -> (Floor, Lift)
exchange   floor_num    (floor, lift) =
  let floor = floor
      to =
      pax =
  in  (floor, {from: floor_num, to: to, pax: })






move : Int -> Int -> Signal Lift
move   from   to  =




render : Floors -> Lifts -> Svg
render   floors    lifts =






main : Html
main =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 100 100" ]
    [ polygon [ fill "#F0AD00", points "61.649,52.782 31.514,82.916 91.783,82.916" ] []
    ]
