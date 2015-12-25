module Lifty.CompareRender where

import Debug
import String               exposing (join)
import Maybe          as M
import Maybe.Extra    as M  exposing ((?))
import List           as L
import List.Extra     as L
import Array          as A
import Set
import Signal         as S
import Animation            exposing (animate)
import Html
import Svg                  exposing (svg, g)
import Svg.Attributes as Sa exposing (x, y, width, height, style, transform)
import Svg.Events           exposing (onClick)

import Lifty.Util                exposing (s_, f_, imapA, imapL, mkM, mkM2)
import Lifty.Render       as R   exposing (rect_, rBg, vbox, style_)
import Lifty.RenderSim    as RS
import Lifty.OneRender    as R1
import Lifty.TwoRender    as R2
import Lifty.OneSimRender as SR1
import Lifty.TwoSimRender as SR2


view nop startAdd endAdd a s = let
  num_floors1 = A.length s.s1.floors
  num_floors2 = A.length s.s2.floors
  num_lifts1 = A.length s.s1.lifts
  num_lifts2 = A.length s.s2.lifts
  w = f_ <| 4 + 7 + num_lifts1 + num_lifts2
  h = f_ <| 1 + max num_floors1 num_floors2
  nop1 = (mkM a (\_-> nop))
  nop2 = (mkM2 a (\_ _-> nop))
  in Html.div [] [style_, svg
    [ x "0", y "0", width (s_ (20 * w)) , height (s_ (20 * h))
    , vbox -2 -0.5 w h ]
    [ g []
        [ g [ style "pointer-events:none"]
            [ rBg num_floors1 num_lifts1
            , R1.rCallBtns num_floors1 nop1
            , R1.rLiftsOne num_floors1 s.s1.lifts s.s1.t nop2
            , RS.rFloorPax s.s1.floors s.s1.t
            , RS.rLiftPax s.s1.lifts s.s1.t
            , RS.rLeavingPax s.s1.leaving s.s1.t ]
        , g [ transform ("translate ("++ toString (5 + A.length s.s1.lifts) ++ ", 0)")
            , style "pointer-events:none"]
            [ rBg num_floors2 num_lifts2
            , R2.rCallBtns num_floors2 s.s2.calls_up s.s2.calls_down nop1 nop1
            , R2.rLiftsTwo num_floors2 s.s2.lifts s.s2.t nop2
            , RS.rFloorPax s.s2.floors s.s2.t
            , RS.rLiftPax s.s2.lifts s.s2.t
            , RS.rLeavingPax s.s2.leaving s.s2.t ]
        , g []
            [ RS.rAddPax (max num_floors1 num_floors2) (6 + num_lifts1 + num_lifts2)
                         s.adding (mkM a startAdd) (mkM a endAdd)
            , RS.rAddingPax (6 + num_lifts1 + num_lifts2) s.adding ]
        ]
    ] ]
