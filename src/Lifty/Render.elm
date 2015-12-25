module Lifty.Render where

import String               exposing (join)
import Maybe          as M
import Maybe.Extra    as M  exposing ((?))
import List           as L
import Array          as A  exposing (Array)
import Signal               exposing (Message)
import Time                 exposing (Time)
import Animation            exposing (Animation, animate)
import Svg                  exposing (Svg, Attribute, rect, g, circle, text', text)
import Svg.Attributes as SA exposing (x, y, width, height, cx, cy, r, class, transform, fill, stroke, strokeWidth, fontSize, viewBox, opacity)
import Svg.Events           exposing (onClick)
import Html                 exposing (Html)

import Lifty.Util exposing (f_, s_, zeroTo, imapA)


type alias Passenger p = { p | dest : Int, x : Animation}

type alias Lift l = { l | busy : Bool, y : Animation }


movexy : number1 -> number2 -> List Svg -> Svg
movexy x y els=
  g [ transform ("translate(" ++ (s_ x) ++ "," ++ (s_ y) ++ ")") ] els

movex : number -> List Svg -> Svg
movex x els = movexy x 0 els

movey : number -> List Svg -> Svg
movey y els = movexy 0 y els

rect_ : number1 -> number2 -> number3 -> number4 -> List Attribute -> Svg
rect_ x' y' w' h' attrs =
  rect ([x (s_ x'), y (s_ y'), width (s_ w'), height (s_ h')] ++ attrs) []

circle_ : number1 -> number2 -> number3 -> List Attribute -> Svg
circle_ cx' cy' r' attrs =
  circle ([cx (s_ cx'), cy (s_ cy'), r (s_ r')] ++ attrs) []

text_ : String -> number1 -> number2 -> List Attribute -> Svg
text_ str x' y' attrs = text' ([x (s_ x'), y (s_ y')] ++ attrs) [text str]

vbox : number1 -> number2 -> number3 -> number4 -> Attribute
vbox x y w h = viewBox <| join " " <| [s_ x, s_ y, s_ w, s_ h]

rLift : Bool -> List Svg
rLift busy =
  [ rect_ 0.1 0.2 0.8 0.8  [fill (if busy then "#ddd" else "#888")]
  , rect_ 0.1 1   0.8 0.04 [fill (if busy then "#fff" else "#ddd")]
  , rect_ 0.1 0.2 0.8 0.04 [fill (if busy then "#fff" else "#ddd")] ]

rLiftBtn : Message -> Bool -> List Svg
rLiftBtn msg is_dest =
  [ rect_ 0.1 0.2 0.8 0.84
          [onClick msg, class (if is_dest then "liftbtn dest" else "liftbtn")] ]

rLifts : (Int -> Lift l -> Bool) -> Int -> Array (Lift l) -> Time -> (Int -> Int -> Message) -> Svg
rLifts is_dest num_floors lifts t goM =
  g [] <| imapA lifts <| \(lift_id, lift) -> movex lift_id
    [ g [] <| flip L.map (zeroTo num_floors) <| \(floor_id) -> movey floor_id <|
         rLiftBtn (goM lift_id floor_id) (is_dest floor_id lift)
    , movey (animate t lift.y) <| rLift lift.busy ]

rBg : Int -> Int -> Svg
rBg num_floors num_lifts = g []
  [ rect_ -2 -0.5 0 0 [width "100%", height "100%", fill "#555"]
  , rect_ (-2) num_floors 0 0.04 [width "100%", fill "white"]
  , g [] <| flip L.map (zeroTo num_floors) <| \(floor_id) ->
      movey floor_id [ rect_ -2 0 0 0.04 [width "100%", fill "white"]
                     , text_ (s_ floor_id) -1.6 0.7 [fontSize "0.5"] ]
  , g [] <| flip L.map (zeroTo num_lifts) <| \(lift_id) ->
      movex lift_id [ rect_ 0.1 0.04  0.8 ((f_ num_floors) - 0.04)
                            [ fill "#000", opacity "0.7"] ] ]

style_ : Html
style_ = Html.node "style" [] [Html.text """
svg { user-select: none; }
svg text { fill: white; }
.addbtn          {cursor:pointer}
.addbtn:hover    {fill: #ddd}
.callbtn         {fill: #084; cursor:pointer}
.callbtn.pressed {fill: #0f8}
.callbtn:hover   {fill: #0f8}
.liftbtn         {fill: transparent; cursor: pointer}
.liftbtn.dest    {fill: #084}
.liftbtn:hover   {fill: #0f8}
"""]
