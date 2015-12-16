module Lifty.OneView where

import String               exposing (join)
import List           as L
import Array          as A
import Signal         as S
import Animation            exposing (animate)
import Svg                  exposing (svg, g, rect, circle, text', text)
import Svg.Attributes as Sa exposing (..)
import Svg.Events           exposing (..)

import Lifty.OneController as C

s_ f = f << toString

rect_ x' y' w' h' = rect [s_ x x', s_ y y', s_ width w', s_ height h'] []


--view : S.Address C.Action -> C.Model {} -> Html.Html
view num_floors num_lifts act address {t, lifts} = let
    view_callbtn floor_id = circle
      [ cx "-0.5", s_ cy (toFloat floor_id + 0.5), r "0.2", fill "green"
      , Sa.cursor "pointer"
      , onClick <| S.message address <| act (C.Call floor_id) ] []

    view_floors = g []
      [ g [] <| A.toList <| A.initialize num_floors <| \floor_id -> g []
        [ rect_ (-2) (toFloat floor_id - 0.02) (num_lifts + 2) 0.04
        , view_callbtn floor_id
        , text' [s_ x (-1.6), s_ y (toFloat floor_id + 0.7), fontSize "0.5"] [s_ text floor_id] ]
      , rect_ (-2) (toFloat num_floors - 0.02) (num_lifts + 2) 0.04 ]

    view_liftbtn id floor_id = rect
      [ s_ x (toFloat id + 0.1), s_ y ((toFloat floor_id) + 0.1), width "0.8", height "0.8", rx "0.2", ry "0.2", fill "#aaa"
      , Sa.cursor "pointer"
      , onClick <| S.message address <| act (C.Go id floor_id) ] []

    view_lifts = g [] <| flip L.map (A.toIndexedList lifts) <| \(id, lift) -> g []
      [ rect [ s_ x (toFloat id + 0.1), s_ y 0, s_ width 0.8, s_ height num_floors, fill "#ccc"] []
      , g [] <| A.toList <| A.initialize (num_floors) <| view_liftbtn id
      , rect [ s_ x (toFloat id + 0.1), s_ y ((animate t lift.ani) + 0.1), width "0.8", height "0.8", rx "0.2", ry "0.2", fill (if lift.busy then "#444" else "black")] [] ]

  in svg
    [ x "0", y "0", width "200", height "200"
    , viewBox <| join " " <| L.map toString [-2, -1, num_lifts+2, num_floors+1] ]
    [ view_floors
    , view_lifts ]
