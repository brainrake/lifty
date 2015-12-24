module Lifty.OneSimRender where

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

import Lifty.Util exposing (s_, f_, imapA, imapL)
import Lifty.OneRender exposing (..)


vPa s p y' = g []
    [ circle [ s_ cx (0.3 + animate s.t p.x)
             , s_ cy (0.69 + y')
             , r "0.3", fill "#08f", stroke "#444", strokeWidth "0.02"] []
    , text' [ s_ x (0.16 + animate s.t p.x)
            , s_ y (0.82 + y')
            , fontSize "0.4", fill "#ddd" ] [text <| toString p.dest] ]

vLiftPax a s =
  g [] <| imapA s.lifts <| \(lift_id, lift) ->
    g [] <| imapL lift.pax <| \(p_id, p) ->
      vPa s p <| animate s.t lift.y

vLeavingPax a s =
  g [] <| flip L.map s.leaving <| \(p) -> vPa s p <| toFloat p.dest

vFloorPax a s =
  g [] <| imapA s.floors <| \(floor_id, floor) ->
    g [] <| flip L.map (L.reverse floor) <| \(p) ->
      vPa s p <| toFloat floor_id

vAddPax a s =
  g [] <| imapA s.floors <| \(floor_id, floor) ->
    let action = s.adding |> M.map (\_ -> a.endadd floor_id)
                          |> M.withDefault (a.startadd floor_id)
    in  g [ onClick <| S.message a.address action, Sa.cursor "pointer", class "addbtn"]
          [ circle [ s_ cx (2.6 + f_ (A.length s.lifts)), s_ cy (0.69 + toFloat floor_id), r "0.3"
                   , fill "#888", strokeWidth "0.02"] []
          , text' [ s_ x (2.47 + f_ (A.length s.lifts)), s_ y (0.82 + toFloat floor_id)
                  , fontSize "0.4", fill "#ddd" ] [text "+"] ]

vAddingPax a s =
  g [] <| M.withDefault [] <| flip M.map s.adding <| \(floor_id) ->
    [ circle [ s_ cx (2.6 + f_ (A.length s.lifts)), s_ cy (0.69 + (toFloat floor_id))
             , r "0.3", fill "#08f", stroke "#444", strokeWidth "0.02"] [] ]


view act go call startadd endadd address s =
  let a = { act = act, go = go, call = call, startadd = startadd, endadd = endadd, address = address }
      w = 2 + 3 + toFloat (A.length s.lifts)
      h = 1 + toFloat (A.length s.floors)
  in Html.div [] [style_, svg
    [ x "0", y "0", s_ width (40 * w) , s_ height (40 * h)
    , vbox (-2) (-0.5) w h ]
    [ vBg
    , vFloors a s
    , vCallBtns a s
    , vLifts a s
    , vAddPax a s
    , vAddingPax a s
    , vFloorPax a s
    , vLiftPax a s
    , vLeavingPax a s
    ] ]