module Lifty.OneRender where

import String               exposing (join)
import List           as L
import Array          as A  exposing (Array)
import Signal         as S  exposing (Message)
import Time                 exposing (Time)
import Svg                  exposing (Svg, svg, g)
import Svg.Attributes       exposing (x, y, width, height, class)
import Svg.Events           exposing (onClick)
import Html

import Lifty.Util           exposing (s_, f_, zeroTo, mkM, mkM2)
import Lifty.Render         exposing (movexy, movex, movey, rect_, circle_, vbox, rLift, rLiftBtn, rLifts, rBg, style_)


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
  w = 2 + f_ num_lifts
  h = 1 + f_ num_floors
  in Html.div [] [style_, svg
    [ x "0", y "0", width (s_ (40 * w)) , height (s_ (40 * h))
    , vbox -2 -0.5 w h ]
    [ rBg num_floors num_lifts
    , rCallBtns num_floors (mkM a call)
    , rLiftsOne num_floors s.lifts s.t (mkM2 a go)
    ] ]
