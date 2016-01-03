module Lifty.TwoSimRender where

import Debug
import String               exposing (join)
import Maybe          as M
import List           as L
import Array          as A
import Signal         as S
import Animation            exposing (animate)
import Html
import Svg                  exposing (svg)
import Svg.Attributes as Sa exposing (x, y, width, height)

import Lifty.Util           exposing (mkM, mkM2)
import Lifty.Render         exposing (rBg, rLifts, rFrame)
import Lifty.RenderSim      exposing (rPax)
import Lifty.TwoRender      exposing (rCallBtns, rLiftsTwo)


render go callUp callDown startAdd endAdd a s = let
  num_floors = A.length s.floors
  num_lifts = A.length s.lifts
  in rFrame (4 + num_lifts) (1 + num_floors)
    [ rBg num_floors num_lifts
    , rCallBtns num_floors s.calls_up s.calls_down (mkM a callUp) (mkM a callDown)
    , rLiftsTwo num_floors s.lifts s.t (mkM2 a go)
    , rPax (mkM a startAdd) (mkM a endAdd) s ]
