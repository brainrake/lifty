module Lifty.OneBuildingView where

import Debug
import String               exposing (join)
import List           as L
import Array          as A
import Signal         as S
import Animation            exposing (animate)
import Html
import Svg                  exposing (svg, g, rect, circle, text', text)
import Svg.Attributes as Sa exposing (..)
import Svg.Events           exposing (..)

import Lifty.OneController as C
import Lifty.OneView exposing (..)

s_ f = f << toString


paxV a m = g [] <|
  [ g [] <| flip L.map (A.toIndexedList m.floors) <| \(floor_id, floor) -> g []
    [ g [] <| flip L.map (A.toIndexedList <| A.fromList floor) <| \(p_id, p) -> g []
        [ circle [ s_ cx (2.4 + (toFloat p_id) / 2), s_ cy (0.69 + toFloat floor_id), r "0.3"
                 , fill "#08f", stroke "#444", strokeWidth "0.02"] []
        , text' [ s_ x (2.3 + toFloat p_id / 2), s_ y (0.83 + toFloat floor_id), fontSize "0.4", fill "#fff" ] [text <| toString p.dest]]
    , g [ onClick <| S.message a.address (a.add floor_id 2) , Sa.cursor "pointer", class "addbtn"]
        [ circle [ s_ cx 4.6, s_ cy (0.69 + toFloat floor_id), r "0.3"
                 , fill "#888", strokeWidth "0.02"] []
        , text' [ s_ x 4.47, s_ y (0.82 + toFloat floor_id)
                , fontSize "0.4", fill "#ddd" ] [text "+"] ] ] ]

liftPaxV a m =
  g [] <| flip L.map (A.toIndexedList m.lifts) <| \(lift_id, lift) ->
    g [] <| flip L.map (A.toIndexedList <| A.fromList lift.pax) <| \(p_id, p) -> g []
      [ circle [ s_ cx (0.3 + toFloat lift_id + (toFloat p_id) / 3)
               , s_ cy (0.7 + animate m.t lift.ani)
               , r "0.3", fill "#08f", stroke "#444", strokeWidth "0.02"] []
      , text' [ s_ x (0.16 + toFloat lift_id + (toFloat p_id) / 3)
              , s_ y (0.82 + animate m.t lift.ani)
              , fontSize "0.4", fill "#ddd" ] [text <| toString p.dest] ]


view act add address m =
  let a = { act = act, add = add, address = address }
      w = 2 + 3 + toFloat (A.length m.lifts)
      h = 1 + toFloat (A.length m.floors)
      --_ = Debug.log "floors" <| toString m.floors
  in Html.div [] [style_, svg
    [ x "0", y "0", s_ width (40 * w) , s_ height (40 * h)
    , vbox (-2) (-0.5) w h ]
    [ bgV
    , floorsV a m
    , liftsV a m
    , liftPaxV a m
    , paxV a m
    ] ]
