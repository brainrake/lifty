module Lifty.OneRender where

import String               exposing (join)
import List           as L
import Array          as A
import Signal         as S
import Animation            exposing (animate)
import Svg                  exposing (svg, g, rect, circle, text', text)
import Svg.Attributes as Sa exposing (..)
import Svg.Events           exposing (..)
import Html

import Lifty.Util exposing (s_, f_, mapiA)
import Lifty.OneController as C


rect_ x' y' w' h' a' =
  rect (L.append [s_ x x', s_ y y', s_ width w', s_ height h'] a') []


style_ = Html.node "style" [] [Html.text """
svg { user-select: none; }
.addbtn:hover circle {fill: #ddd}
.callbtn         {fill: #084; cursor:pointer}
.callbtn.pressed {fill: #0f8}
.callbtn:hover   {fill: #0f8}
.liftbtn:hover   {fill: #0f8}
"""]

vCallBtns a m =
  g [] <| mapiA m.floors <| \(floor_id, _)  -> circle
    [ class "callbtn", cx "-0.5", s_ cy (f_ floor_id + 0.5), r "0.2"
    , Sa.cursor "pointer"
    , onClick <| S.message a.address <| a.act (a.call floor_id) ] []

vFloors a m = g []
  [ rect_ (-2) (f_ <| A.length m.floors) 0 0.04 [width "100%", fill "white"]
  , g [] <| mapiA m.floors <| \(floor_id, _)  -> g []
    [ rect_ (-2) (f_ floor_id) 0 0.04 [width "100%", fill "white"]
    , text' [s_ x (-1.6), s_ y (f_ floor_id + 0.7), fontSize "0.5", fill "white"]
            [s_ text floor_id] ]]

vLiftBtn a m lift id floor_id =
  rect_ (f_ id + 0.1) ((f_ floor_id) + 0.2) 0.8 0.84
        [ fill (if floor_id == lift.dest then "#084" else "transparent")
        , class "liftbtn", Sa.cursor "pointer"
        , onClick <| S.message a.address <| a.act (a.go id floor_id) ]

vLift a m lift_id lift = g []
  [ rect_ (f_ lift_id + 0.1) ((animate m.t lift.y) + 0.2) 0.8 0.8
          [ fill (if lift.busy then "#ddd" else "#888") ]
  , rect_ (f_ lift_id + 0.1) ((animate m.t lift.y) + 1) 0.8 0.04
          [ fill (if lift.busy then "#fff" else "#ddd") ]
  , rect_ (f_ lift_id + 0.1) ((animate m.t lift.y) + 0.2) 0.8 0.04
          [ fill (if lift.busy then "#fff" else "#ddd")] ]


vLifts a m =
  g [] <| mapiA m.lifts <| \(lift_id, lift) -> g []
    [ rect_ (f_ lift_id + 0.1) 0.04  0.8 ((f_ <| A.length m.floors) - 0.04)
            [ fill "#000", opacity "0.7"]
    , g [] <| mapiA m.floors <| \(floor_id, _) ->
        vLiftBtn a m lift lift_id floor_id
    , vLift a m lift_id lift ]

vBg = rect [x "-2", y "-0.5", width "100%", height "100%", fill "#555"] []

vbox x y w h = viewBox <| join " " <| L.map toString [x, y, w, h]

-- g [transform ("translate(0 " ++ (toString (h-1)) ++ ") scale(1 -1)")] [

view act go call address m =
  let a = { act = act, go = go, call = call, address = address }
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
