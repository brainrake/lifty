module Lifty.TwoRender where

import Debug
import String               exposing (join)
import Maybe          as M
import List           as L
import Array          as A
import Set                  exposing (Set)
import Signal         as S
import Animation            exposing (animate)
import Html
import Svg                  exposing (svg, g, rect, circle, text', text)
import Svg.Attributes as Sa exposing (..)
import Svg.Events           exposing (..)

import Lifty.Util exposing (f_, s_, imapA, rect_)
import Lifty.TwoController as C
import Lifty.OneRender exposing (..)


vCallBtns a m =
  g [] <| imapA m.floors <| \(floor_id, _)  ->
    g [] [ circle
            [ cx "-0.5", s_ cy (f_ floor_id + 0.72), r "0.2"
            , class (if Set.member floor_id m.calls_up then "callbtn pressed" else "callbtn")
            , opacity (if floor_id == ((A.length m.floors) - 1) then "0" else "1")
            , onClick <| S.message a.address <| a.act (a.callup floor_id) ] []
         , circle
            [ cx "-0.5", s_ cy (f_ floor_id + 0.28), r "0.2"
            , class (if Set.member floor_id m.calls_down then "callbtn pressed" else "callbtn")
            , opacity (if floor_id == 0 then "0" else "1")
            , onClick <| S.message a.address <| a.act (a.calldown floor_id) ] [] ]


vLiftBtn a m lift id floor_id =
  rect_ (f_ id + 0.1) ((f_ floor_id) + 0.2) 0.8 0.84
        [ fill (if Set.member floor_id lift.dests then "#084" else "transparent")
        , class "liftbtn", Sa.cursor "pointer"
        , onClick <| S.message a.address <| a.act (a.go id floor_id) ]

vLifts a m =
  g [] <| imapA m.lifts <| \(lift_id, lift) -> g []
    [ rect_ (f_ lift_id + 0.1) 0.04  0.8 ((f_ <| A.length m.floors) - 0.04)
            [ fill "#000", opacity "0.7"]
    , g [] <| imapA m.floors <| \(floor_id, _) ->
        vLiftBtn a m lift lift_id floor_id
    , rect_ (0.45 + f_ lift_id) (0.55 + f_ lift.next) 0.1 0.1 [fill "#444"]
    , vLift a m lift_id lift ]


view act go callup calldown address m =
  let a = { act = act, go = go, callup = callup, calldown = calldown, address = address }
      w = 2 + f_ (A.length m.lifts)
      h = 1 + f_ (A.length m.floors)
  in Html.div [] [style_, svg
    [ x "0", y "0", s_ width (40 * w) , s_ height (40 * h)
    , vbox (-2) (-0.5) w h ]
    [ vBg
    , vFloors a m
    , vCallBtns a m
    , vLifts a m

    ] ]
