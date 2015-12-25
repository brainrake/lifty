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

import Lifty.Util           exposing (s_, f_, mkM, mkM2)
import Lifty.Render         exposing (rect_, circle_, text_, movey, rBg, rLifts, vbox, style_)
import Lifty.RenderSim      exposing (rPax)
import Lifty.TwoRender      exposing (rCallBtns, rLiftsTwo)


render go callUp callDown startAdd endAdd a s = let
  num_floors = A.length s.floors
  num_lifts = A.length s.lifts
  w = 2 + 3 + f_ num_lifts
  h = 1 + f_ num_floors
  in Html.div [] [style_, svg
    [ x "0", y "0", width (s_ (40 * w)), height (s_ (40 * h))
    , vbox (-2) (-0.5) w h ]
    [ rBg num_floors num_lifts
    , rCallBtns num_floors s.calls_up s.calls_down (mkM a callUp) (mkM a callDown)
    , rLiftsTwo num_floors s.lifts s.t (mkM2 a go)
    , rPax (mkM a startAdd) (mkM a endAdd) s
    ] ]
