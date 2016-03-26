module Lifty.AllRender where

import List           as L
import List.Extra     as L
import Array          as A  exposing (Array)
import Signal         as S  exposing (Message)
import Time                 exposing (Time)
import Svg                  exposing (Svg, g)
import Svg.Attributes       exposing (fill)
import Animation            exposing (animate)

import Lifty.Util           exposing (s_, f_, imapA, zeroTo, mkM)
import Lifty.RenderUtil     exposing (rect_, movex, movey)
import Lifty.Render         exposing (rBg, rLifts, rLift, rFrame)
import Lifty.RenderSim      exposing (rAddPax, rAddingPax, rFloorPax, rLiftPax, rLeavingPax)


type alias Lift l = Lifty.Render.Lift { l | next : Int, busy : Bool }

--rLiftsAll : Int -> Array (Lift l) -> Time -> Svg
rLiftsAll num_floors lifts t =
  g [] <| imapA lifts <| \(lift_id, lift) -> movex lift_id
    [ g [] <| flip L.map (zeroTo num_floors) <| \(floor_id) -> movey floor_id <|
        [ rect_ 0.1 0 0.8 0.84
                [fill (if floor_id == lift.next then "#084" else "transparent")] ]
    , movey (animate t lift.y) <| rLift lift.busy ]


view startAdd endAdd a s =
  let num_floors = A.length s.floors
      num_lifts = A.length s.lifts
      floors = A.fromList (zeroTo num_floors |> L.map (\fi -> L.filter (\p -> p.src == fi) s.pax))
  in rFrame (5 + num_lifts) (1 + num_floors)
    [ rBg num_floors num_lifts
    , rLiftsAll num_floors s.lifts s.t
    , rAddPax num_floors num_lifts s.adding (mkM a startAdd) (mkM a endAdd)
    , rAddingPax num_lifts s.adding
    , rFloorPax floors s.t
    , rLiftPax num_floors s.lifts s.t
    , rLeavingPax num_floors s.leaving s.t ]
