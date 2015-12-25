module Lifty.RenderSim where

import Maybe          as M
import Maybe.Extra    as M  exposing ((?))
import List           as L
import Array          as A  exposing (Array)
import Signal         as S  exposing (Message)
import Time                 exposing (Time)
import Animation            exposing (Animation, animate)
import Svg                  exposing (Svg, g)
import Svg.Attributes       exposing (class, style, fill, stroke, strokeWidth, fontSize)
import Svg.Events           exposing (onClick)

import Lifty.Util           exposing (s_, f_, zeroTo, imapA, imapL, mkM, mkM2)
import Lifty.Render         exposing (Passenger, movexy, movex, movey, circle_, text_, rBg, style_, vbox)


type alias Lift l p = { l | pax : List (Passenger p), y : Animation }


rPa : Passenger p -> Time -> Svg
rPa p t =
  movex (animate t p.x)
    [ circle_ 0.3 0.69 0.3 [fill "#06c", stroke "#444", strokeWidth "0.02"]
    , text_ (s_ p.dest) 0.16 0.82 [fontSize "0.4", fill "#ddd" ] ]

rLiftPax : Array (Lift l p) -> Time -> Svg
rLiftPax lifts t =
  g [] <| imapA lifts <| \(lift_id, lift) ->
    g [] <| imapL lift.pax <| \(p_id, p) ->
      movey (animate t lift.y) [rPa p t]

rLeavingPax : List (Passenger p) -> Time -> Svg
rLeavingPax leaving t =
  g [] <| flip L.map leaving <| \(p) -> movey p.dest [rPa p t]

rFloorPax : Array (List (Passenger p)) -> Time -> Svg
rFloorPax floors t =
  g [] <| imapA floors <| \(floor_id, floor) ->
    g [] <| flip L.map (L.reverse floor) <| \(p) ->
      movey floor_id [rPa p t]

rAddPax : Int -> Int -> Maybe Int -> (Int -> Message) -> (Int -> Message) -> Svg
rAddPax num_floors num_lifts adding startAddM endAddM =
  g [] <| flip L.map (zeroTo num_floors) <| \(floor_id) ->
    let msg = ((adding |> M.map (\_ -> endAddM)) ? startAddM) floor_id
    in movey floor_id
      [ circle_ (2.6 + f_ num_lifts) 0.69 0.3 [ fill "#888", strokeWidth "0.02"
                                              , onClick msg, class "addbtn"]
      , text_ "+" (2.47 + f_ num_lifts) 0.82
              [fontSize "0.4", fill "#ddd", style "pointer-events:none"] ]

rAddingPax : Int -> Maybe Int -> Svg
rAddingPax num_lifts adding =
  M.withDefault (g [] []) <| flip M.map adding <| \(floor_id) ->
    movexy num_lifts floor_id
      [ circle_ 2.6 0.69 0.3 [fill "#08f", stroke "#444", strokeWidth "0.02"] ]

rPax startAddM endAddM s = g []
  [ rAddPax (A.length s.floors) (A.length s.lifts) s.adding startAddM endAddM
  , rAddingPax (A.length s.lifts) s.adding
  , rFloorPax s.floors s.t
  , rLiftPax s.lifts s.t
  , rLeavingPax s.leaving s.t]
