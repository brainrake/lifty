module Lifty.OneView where

import String               exposing (join)
import List           as L
import Array          as A
import Signal         as S
import Animation            exposing (animate)
import Svg                  exposing (svg, g, rect, circle, text', text)
import Svg.Attributes as Sa exposing (..)
import Svg.Events           exposing (..)
import Html

import Lifty.OneController as C

s_ f = f << toString

rect_ x' y' w' h' a' =
  rect (L.append a' [s_ x x', s_ y y', s_ width w', s_ height h']) []


style_ = Html.node "style" [] [Html.text """
svg { user-select: none; }
.addbtn:hover circle {fill: #ddd}
.callbtn       {fill: #084}
.callbtn:hover {fill: #0f8}
.liftbtn:hover {fill: #0f8}
"""]

callbtnV a m floor_id = circle
  [ class "callbtn", cx "-0.5", s_ cy (toFloat floor_id + 0.5), r "0.2"
  , Sa.cursor "pointer"
  , onClick <| S.message a.address <| a.act (C.Call floor_id) ] []

floorsV a m = g []
  [ g [] <| flip L.map (A.toIndexedList m.floors) <| \(floor_id, _)  -> g []
    [ rect [x "-2", s_ y (toFloat floor_id), width "100%", height "0.04", fill "white"] []
    , callbtnV a m floor_id
    , text' [s_ x (-1.6), s_ y (toFloat floor_id + 0.7), fontSize "0.5", fill "white"] [s_ text floor_id] ]
  , rect [x "-2", s_ y (toFloat <| A.length m.floors), width "100%", height "0.04", fill "white"] [] ]

liftbtnV a m lift id floor_id = rect
  [ s_ x (toFloat id + 0.1), s_ y ((toFloat floor_id) + 0.2), width "0.8", height "0.84"
  , fill (if floor_id == lift.dest then "#084" else "transparent")
  , class "liftbtn", Sa.cursor "pointer"
  , onClick <| S.message a.address <| a.act (C.Go id floor_id) ] []

liftsV a m =
  g [] <| flip L.map (A.toIndexedList m.lifts) <| \(lift_id, lift) -> g []
    [ rect [ s_ x (toFloat lift_id + 0.1), s_ y 0.04, s_ width 0.8, s_ height ((toFloat <| A.length m.floors) - 0.04), fill "#000", opacity "0.7"] []
    , g [] <| flip L.map (A.toIndexedList m.floors) <| \(floor_id, _) ->
        liftbtnV a m lift lift_id floor_id
    , rect_ (toFloat lift_id + 0.1) ((animate m.t lift.ani) + 0.2) 0.8 0.8
            [ fill (if lift.busy then "#ddd" else "#888")]
    , rect [ s_ x (toFloat lift_id + 0.1), s_ y ((animate m.t lift.ani) + 1)
           , width "0.8", height "0.04"
           , fill (if lift.busy then "#fff" else "#ddd")] []
    , rect [ s_ x (toFloat lift_id + 0.1), s_ y ((animate m.t lift.ani) + 0.2)
           , width "0.8", height "0.04"
           , fill (if lift.busy then "#fff" else "#ddd")] [] ]


bgV = rect [x "-2", y "-0.5", width "100%", height "100%", fill "#555"] []

vbox x y w h = viewBox <| join " " <| L.map toString [x, y, w, h]

view act address m =
  let a = { act = act, address = address }
      w = 2 + toFloat (A.length m.lifts)
      h = 1 + toFloat (A.length m.floors)
      --_ = Debug.log "floors" <| toString m.floors
  in Html.div [] [style_, svg
    [ x "0", y "0", s_ width (40 * w) , s_ height (40 * h)
    , vbox (-2) (-0.5) w h ]
    [ bgV
    , floorsV a m
    , liftsV a m
    ] ]
