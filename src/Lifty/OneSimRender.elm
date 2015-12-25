module Lifty.OneSimRender where

import Debug
import String               exposing (join)
import Maybe          as M
import Maybe.Extra    as M  exposing ((?))
import List           as L
import Array          as A  exposing (Array)
import Signal         as S  exposing (Message)
import Time                 exposing (Time)
import Animation            exposing (Animation, animate)
import Html
import Svg                  exposing (Svg, svg, g, rect, circle, text', text)
import Svg.Attributes       exposing (x, y, width, height, class, fill, stroke, strokeWidth, fontSize)
import Svg.Events           exposing (onClick)

import Lifty.Util           exposing (s_, f_, zeroTo, imapA, imapL, mkM, mkM2)
import Lifty.Render         exposing (Passenger, movexy, movex, movey, circle_, text_, rBg, style_, vbox)
import Lifty.RenderSim      exposing (rPax)
import Lifty.OneRender      exposing (rCallBtns, rLiftsOne)


render go call startAdd endAdd a s = let
  num_floors = A.length s.floors
  num_lifts = A.length s.lifts
  w = 1 + 3 + f_ num_lifts
  h = 1 + f_ num_floors
  in Html.div [] [style_, svg
    [ x "0", y "0", width (s_ (40 * w)) , height (s_ (40 * h))
    , vbox (-1) (-0.5) w h ]
    [ rBg num_floors num_lifts
    , rCallBtns num_floors (mkM a call)
    , rLiftsOne num_floors s.lifts s.t (mkM2 a go)
    , rPax (mkM a startAdd) (mkM a endAdd) s
    ] ]
