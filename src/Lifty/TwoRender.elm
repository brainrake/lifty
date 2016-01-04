module Lifty.TwoRender where

import Maybe          as M
import List           as L
import Array          as A
import Set                  exposing (Set)
import Signal         as S  exposing (Message)
import Svg                  exposing (Svg, g, polygon)
import Svg.Attributes       exposing (class, opacity, points, fill)
import Svg.Events           exposing (onClick)
import Animation            exposing (animate)

import Lifty.Util           exposing (zeroTo, mkM, mkM2, imapA)
import Lifty.RenderUtil     exposing (circle_, movexy, movex, movey)
import Lifty.Render         exposing (rBg, rLift, rLiftBtn, rFrame)


rCallBtns : Int -> Set Int -> Set Int -> (Int -> Message) -> (Int -> Message) -> Svg
rCallBtns num_floors calls_up calls_down callUpM callDownM =
  g [] <| flip L.map (zeroTo num_floors) <| \(floor_id) ->
    movey floor_id
      [ circle_ -0.5 0.72 0.2
          [ class (if Set.member floor_id calls_up then "callbtn pressed"
                                                   else "callbtn")
          , opacity (if floor_id == (num_floors - 1) then "0" else "1")
          , onClick (callUpM floor_id) ]
      , circle_ -0.5 0.28 0.2
          [ class (if Set.member floor_id calls_down then "callbtn pressed"
                                                     else "callbtn")
          , opacity (if floor_id == 0 then "0" else "1")
          , onClick (callDownM floor_id) ] ]

rLiftsTwo num_floors lifts t goM =
  g [] <| imapA lifts <| \(lift_id, lift) -> movex lift_id
    [ g [] <| flip L.map (zeroTo num_floors) <| \(floor_id) -> movey floor_id <|
         rLiftBtn (goM lift_id floor_id) (Set.member floor_id lift.dests)
    , movey (animate t lift.y) <|
      [ g [] (rLift lift.busy)
      , polygon [ fill "#084", points
          (if lift.up then "0.3,0.84  0.7,0.84  0.5,1.04"
                      else "0.3,-0.04 0.7,-0.04 0.5,-0.24")] [] ] ]

render go callUp callDown a s = let
  num_floors = A.length s.floors
  num_lifts = A.length s.lifts
  in rFrame (2 + num_lifts) (1 + num_floors)
    [ rBg num_floors num_lifts
    , rCallBtns num_floors s.calls_up s.calls_down (mkM a callUp) (mkM a callDown)
    , rLiftsTwo num_floors s.lifts s.t (mkM2 a go) ]
