module Lifty.OneSimRender where

import Array          as A  exposing (Array)

import Lifty.Util           exposing (s_, f_, zeroTo, imapA, imapL, mkM, mkM2)
import Lifty.Render         exposing (Passenger, rFrame, rBg)
import Lifty.RenderSim      exposing (rPax)
import Lifty.OneRender      exposing (rCallBtns, rLiftsOne)


render go call startAdd endAdd a s = let
  num_floors = A.length s.floors
  num_lifts = A.length s.lifts
  in rFrame (4 + num_lifts) (1 + num_floors)
    [ rBg num_floors num_lifts
    , rCallBtns num_floors (mkM a call)
    , rLiftsOne num_floors s.lifts s.t (mkM2 a go)
    , rPax (mkM a startAdd) (mkM a endAdd) s ]
