module Lifty.TwoView where

import Debug
import String               exposing (join)
import Maybe          as M
import List           as L
import Array          as A
import Signal         as S
import Animation            exposing (animate)
import Html
import Svg                  exposing (svg, g, rect, circle, text', text)
import Svg.Attributes as Sa exposing (..)
import Svg.Events           exposing (..)

import Lifty.Util exposing (f_, s_, mapiA, mapiL)
import Lifty.TwoController as C
import Lifty.OneView exposing (..)


vCallBtns a m =
  g [] <| mapiA m.floors <| \(floor_id, _)  ->
    g [] [ circle
            [ class "callbtn", cx "-0.5", s_ cy (f_ floor_id + 0.28), r "0.2"
            , Sa.cursor "pointer"
            , onClick <| S.message a.address <| a.act (C.CallUp floor_id) ] []
         , circle
            [ class "callbtn", cx "-0.5", s_ cy (f_ floor_id + 0.72), r "0.2"
            , Sa.cursor "pointer"
            , onClick <| S.message a.address <| a.act (C.CallDown floor_id) ] [] ]


view act address m =
  let a = { act = act, go = C.Go, address = address }
      w = 2 + f_ (A.length m.lifts)
      h = 1 + f_ (A.length m.floors)
      --_ = Debug.log "floors" <| toString m.floors
  in Html.div [] [style_, svg
    [ x "0", y "0", s_ width (40 * w) , s_ height (40 * h)
    , vbox (-2) (-0.5) w h ]
    [ vBg
    , vFloors a m
    , vCallBtns a m
    , vLifts a m
    ] ]
