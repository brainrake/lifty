module Lifty.TwoSimRender where

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

import Lifty.Util           exposing (s_, f_, imapA, imapL)
import Lifty.OneRender      exposing (..)
import Lifty.TwoRender as R exposing (..)
--import Lifty.TwoSimRender exposing (..)


vPa m p y' = g []
    [ circle [ s_ cx (0.3 + animate m.t p.x)
             , s_ cy (0.69 + y')
             , r "0.3", fill "#08f", stroke "#444", strokeWidth "0.02"] []
    , text' [ s_ x (0.16 + animate m.t p.x)
            , s_ y (0.82 + y')
            , fontSize "0.4", fill "#ddd" ] [text <| toString p.dest] ]

vLiftPax a m =
  g [] <| imapA m.lifts <| \(lift_id, lift) ->
    g [] <| imapL lift.pax <| \(p_id, p) ->
      vPa m p <| animate m.t lift.y

vLeavingPax a m =
  g [] <| flip L.map m.leaving <| \(p) -> vPa m p <| f_ p.dest

vFloorPax a m =
  g [] <| imapA m.floors <| \(floor_id, floor) ->
    g [] <| flip L.map (L.reverse floor) <| \(p) ->
      vPa m p <| f_ floor_id

vAddPax a m =
  g [] <| imapA m.floors <| \(floor_id, floor) ->
    let action = m.adding |> M.map (\_ -> a.endadd floor_id)
                          |> M.withDefault (a.startadd floor_id)
    in  g [ onClick <| S.message a.address action
          , Sa.cursor "pointer", class "addbtn"]
          [ circle [ s_ cx (2.6 + f_ (A.length m.lifts))
                   , s_ cy (0.69 + (f_ floor_id)), r "0.3"
                   , fill "#888", strokeWidth "0.02"] []
          , text' [ s_ x (2.47 + f_ (A.length m.lifts))
                  , s_ y (0.82 + (f_ floor_id))
                  , fontSize "0.4", fill "#ddd" ] [text "+"] ]

vAddingPax a m =
  g [] <| M.withDefault [] <| flip M.map m.adding <| \(floor_id) ->
    [ circle [ s_ cx (2.6 + f_ (A.length m.floors)), s_ cy (0.69 + (f_ floor_id))
             , r "0.3", fill "#08f", stroke "#444", strokeWidth "0.02"] [] ]


view act go callup calldown startadd endadd address m =
  let a = { act = act, go = go, callup = callup, calldown = calldown, startadd = startadd, endadd = endadd, address = address }
      w = 2 + 3 + f_ (A.length m.lifts)
      h = 1 + f_ (A.length m.floors)
  in Html.div [] [style_, svg
    [ x "0", y "0", s_ width (40 * w) , s_ height (40 * h)
    , vbox (-2) (-0.5) w h ]
    [ vBg
    , vFloors a m
    , R.vCallBtns a m
    , R.vLifts a m
    , vAddPax a m
    , vAddingPax a m
    , vFloorPax a m
    , vLiftPax a m
    , vLeavingPax a m
    ] ]
