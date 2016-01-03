module Lifty.OneRender where

import List           as L
import Array          as A  exposing (Array)
import Signal         as S  exposing (Message)
import Svg                  exposing (Svg, g)
import Svg.Attributes       exposing (class)
import Svg.Events           exposing (onClick)
import Html

import Lifty.Util           exposing (s_, f_, zeroTo, mkM, mkM2)
import Lifty.RenderUtil     exposing (movey, circle_)
import Lifty.Render         exposing (rFrame, rLift, rLiftBtn, rLifts, rBg, style_)


type alias Lift l = Lifty.Render.Lift { l | dest : Int }


rCallBtns : Int -> (Int -> Message) -> Svg
rCallBtns num_floors callM =
  g [] <| flip L.map (zeroTo num_floors) <| \(floor_id)  ->
    movey floor_id [ circle_ -0.5 0.5 0.2 [ class "callbtn"
                                          , onClick <| callM floor_id ] ]

rLiftsOne = rLifts (\fi l -> fi == l.dest)

render go call a s = let
  num_floors = A.length s.floors
  num_lifts = A.length s.lifts
  in rFrame (2 + num_lifts) (1 + num_floors)
    [ rBg num_floors num_lifts
    , rCallBtns num_floors (mkM a call)
    , rLiftsOne num_floors s.lifts s.t (mkM2 a go) ]
