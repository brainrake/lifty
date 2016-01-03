module Lifty.CompareRender where

import Debug
import String                exposing (join)
import Maybe           as M
import Maybe.Extra     as M  exposing ((?))
import List            as L
import List.Extra      as L
import Array           as A
import Set
import Signal          as S
import Json.Decode     as JD
import Json.Encode     as JE
import Animation             exposing (animate)
import Html            as H
import Html.Attributes as H
import Html.Events     as HE
import Svg                   exposing (svg, g)
import Svg.Attributes        exposing (x, y, width, height, style, transform)
import Svg.Events            exposing (onClick)
import Svg.Lazy        as SL

import Lifty.Util                exposing (s_, f_, imapA, imapL, mkM, mkM2)
import Lifty.RenderUtil          exposing (rect_, vbox)
import Lifty.Render       as R   exposing (rBg, style_)
import Lifty.RenderSim    as RS
import Lifty.OneRender    as R1
import Lifty.TwoRender    as R2
import Lifty.OneSimRender as SR1
import Lifty.TwoSimRender as SR2


rUI toggleRandomM fillM = H.div []
  [ H.input [ H.id "addrandom", H.type' "checkbox", H.checked True
            , HE.on "change" HE.targetChecked toggleRandomM] []
  , H.label [H.for "addrandom"] [H.text "Add random passengers"]
  , H.span [ H.property "innerHTML" <| JE.string " &nbsp; &nbsp; " ] []
  , H.button [HE.on "click" JD.value fillM] [ H.text "Fill"]
  ]


lAddPax params = let
  f (nf1, nf2, nl1, nl2, adding, a, startAdd, endAdd) =
    RS.rAddPax (max nf1 nf2) (6 + nl1 + nl2) adding (mkM a startAdd) (mkM a endAdd)
  in SL.lazy f params

view startAdd endAdd toggleRandom fill nop a s = let
  num_floors1 = A.length s.s1.floors
  num_floors2 = A.length s.s2.floors
  num_lifts1 = A.length s.s1.lifts
  num_lifts2 = A.length s.s2.lifts
  w = f_ <| 2 + 8 + num_lifts1 + num_lifts2
  h = f_ <| 1 + max num_floors1 num_floors2
  nop1 = (mkM a (\_-> nop))
  nop2 = (mkM2 a (\_ _-> nop))
  my_svg = \_-> svg
    [ x "0", y "0", width (s_ (40 * w)) , height (s_ (40 * h))
    , vbox -1 (0.5 - h) w h ]
    [ g [ transform "scale(1,-1)" ]
      [ g [ style "pointer-events:none"]
          [ SL.lazy2 rBg num_floors1 num_lifts1
          , SL.lazy2 R1.rCallBtns num_floors1 nop1
          , R1.rLiftsOne num_floors1 s.s1.lifts s.s1.t nop2
          , RS.rFloorPax s.s1.floors s.s1.t
          , RS.rLiftPax num_floors1 s.s1.lifts s.s1.t
          , RS.rLeavingPax num_floors1 s.s1.leaving s.s1.t ]
      , g [ style "pointer-events:none"
          , transform ("translate ("++ toString (5 + A.length s.s1.lifts) ++ ", 0)")]
          [ SL.lazy2 rBg num_floors2 num_lifts2
          , R2.rCallBtns num_floors2 s.s2.calls_up s.s2.calls_down nop1 nop1
          , R2.rLiftsTwo num_floors2 s.s2.lifts s.s2.t nop2
          , RS.rFloorPax s.s2.floors s.s2.t
          , RS.rLiftPax num_floors2 s.s2.lifts s.s2.t
          , RS.rLeavingPax num_floors2 s.s2.leaving s.s2.t ]
      , g []
          [ lAddPax (num_floors1, num_floors2, num_lifts1, num_lifts2
                    , s.adding, a, startAdd, endAdd)
          --, RS.rAddPax (max num_floors1 num_floors2) (5 + num_lifts1 + num_lifts2)
          --             s.adding (mkM a startAdd) (mkM a endAdd)
          , RS.rAddingPax (6 + num_lifts1 + num_lifts2) s.adding ] ] ]
  in H.div []
           [ style_
           , SL.lazy my_svg s.s1.t
           , rUI (mkM a toggleRandom) (mkM a (\_->fill))]
