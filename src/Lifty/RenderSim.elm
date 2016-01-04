module Lifty.RenderSim where

import Maybe          as M
import Maybe.Extra    as M   exposing ((?))
import List           as L
import Array          as A   exposing (Array)
import Signal         as S   exposing (Message)
import Time                  exposing (Time)
import Animation      as Ani exposing (Animation, animate, ease)
import Easing                exposing (easeInExpo)
import Svg                   exposing (Svg, g)
import Svg.Attributes        exposing (class, style, transform, fill, stroke, strokeWidth, fontSize, opacity)
import Svg.Events            exposing (onClick)
import Svg.Lazy       as SL

import Lifty.Util            exposing (s_, f_, zeroTo, imapA, imapL, mkM, mkM2)
import Lifty.RenderUtil      exposing (movexy, movex, movey, rect_, circle_, text_)
import Lifty.Render          exposing (Passenger, rBg)


type alias Lift l p = { l | pax : List (Passenger p), y : Animation }


rPa : Int -> Passenger p -> Time -> Svg
rPa num_floors p t =
  movex (animate t p.x) [
    (num_floors, p.dest) |> SL.lazy (\(num_floors, dest) ->
        g [transform ("scale(0.16,"++ (s_ (0.8 / f_ num_floors)) ++")")]
          [ rect_ 0 0 1 num_floors [ fill "#1c1c1c" ]
          , g [] <| flip L.map (zeroTo num_floors) <| \fi ->
              rect_ 0 (f_ fi - 0.1) 1 0.2 [ fill "#888"]
          , rect_ 0 dest 1 1 [ fill "#084" ] ])]

rLiftPax : Int -> Array (Lift l p) -> Time -> Svg
rLiftPax num_floors lifts t =
  g [] <| imapA lifts <| \(lift_id, lift) ->
    g [] <| imapL lift.pax <| \(p_id, p) ->
      movey (animate t lift.y) [rPa num_floors p t]

rLeavingPax : Int -> List (Passenger p) -> Time -> Svg
rLeavingPax num_floors leaving t =
  g [] <| flip L.map leaving <| \(p) ->
    (animate t p.x) |> SL.lazy (\_->
      g [ opacity (s_ ( animate t (
            p.x |> Ani.from 1 |> Ani.to 0 |> ease easeInExpo )))]
        [ movey p.dest [rPa num_floors p t] ])

rFloorPax : Array (List (Passenger p)) -> Time -> Svg
rFloorPax floors t =
  g [] <| imapA floors <| \(floor_id, floor) ->
    g [] <| flip L.map (L.reverse floor) <| \(p) ->
      movey floor_id [rPa (A.length floors) p t]

rAddPax : Int -> Int -> Maybe Int -> (Int -> Message) -> (Int -> Message) -> Svg
rAddPax num_floors num_lifts adding startAddM endAddM =
  g [] <| flip L.map (zeroTo num_floors) <| \(floor_id) ->
    let msg = ((adding |> M.map (\_ -> endAddM)) ? startAddM) floor_id
    in movexy num_lifts floor_id
      [ rect_ 2.4 0 0.4 0.8 [ fill "#888", strokeWidth "0.02"
                                              , onClick msg, class "addbtn"]
      , text_ "+" 2.47 0.52
              [fontSize "0.4", fill "#ddd", style "pointer-events:none"] ]

rAddingPax : Int -> Maybe Int -> Svg
rAddingPax num_lifts adding =
  M.withDefault (g [] []) <| flip M.map adding <| \(floor_id) ->
    movexy num_lifts floor_id
      [ rect_ 2.4 0 0.4 0.8 [fill "#084"] ]

rPax startAddM endAddM s = let
  num_floors = A.length s.floors
  num_lifts = A.length s.lifts
  in g []
       [ rAddPax num_floors num_lifts s.adding startAddM endAddM
       , rAddingPax num_lifts s.adding
       , rFloorPax s.floors s.t
       , rLiftPax num_floors s.lifts s.t
       , rLeavingPax num_floors s.leaving s.t]
