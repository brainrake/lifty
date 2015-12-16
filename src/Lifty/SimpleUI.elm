module Lifty.SimpleUI where

import Debug                exposing (log)
--import Time
import String               exposing (join)
import List                 exposing (map)
import Array          as A
import Signal         as S
import Signal.Extra   as SE exposing ((<~), (~), (~>))
import Task
import Effects              exposing (Effects, Never, task)
import StartApp             exposing (start)
import Html
import Svg                  exposing (..)
import Svg.Attributes       exposing (..)
import Svg.Events           exposing (..)

import Lifty.SimpleController as C


s_ f = f << toString

rect_ x' y' w' h' = rect [s_ x x', s_ y y', s_ width w', s_ height h'] []

view_callbtn address floor_id = circle
  [ cx "-0.5", s_ cy (toFloat floor_id + 0.5), r "0.2", fill "green"
  , Svg.Attributes.cursor "pointer"
  , onClick <| S.message address <| CAction (C.Call floor_id) ] []

view_floors address = g []
  [ g [] <| A.toList <| A.initialize num_floors <| \floor_id -> g []
    [ rect_ (-2) (toFloat floor_id - 0.02) (num_lifts + 2) 0.04
    , view_callbtn address floor_id
    , text' [s_ x (-1.6), s_ y (toFloat floor_id + 0.7), fontSize "0.5"] [s_ text floor_id] ]
  , rect_ (-2) (toFloat num_floors - 0.02) (num_lifts + 2) 0.04 ]

view_liftbtn address id floor_id = rect
  [ s_ x (toFloat id + 0.1), s_ y ((toFloat floor_id) + 0.1), width "0.8", height "0.8", rx "0.2", ry "0.2", fill "#aaa"
  , Svg.Attributes.cursor "pointer"
  , onClick <| S.message address <| CAction (C.Go id floor_id) ] []

view_lifts address lifts = g [] <| flip map (A.toIndexedList lifts) <| \(id, lift) -> g []
  [ rect [ s_ x (toFloat id + 0.1), s_ y 0, s_ width 0.8, s_ height num_floors, fill "#ccc"] []
  , g [] <| A.toList <| A.initialize (num_floors) <| view_liftbtn address id
  , rect [ s_ x (toFloat id + 0.1), s_ y ((toFloat lift.dest) + 0.1), width "0.8", height "0.8", rx "0.2", ry "0.2", fill (if lift.moving then "#888" else "black")] [] ]

--view : S.Address Action -> C.Model {} -> Html.Html
view address model = svg
  [ x "0", y "0", width "200", height "200"
  , viewBox <| join " " <| map toString [-2, -1, num_lifts+2, num_floors+1] ]
  [ view_floors address
  , view_lifts address model ]



type Action = CAction C.Action | CEvent C.Event

num_floors = 5
num_lifts = 2

init_model : C.Model { y : Float }
init_model = A.repeat num_lifts { dest = 0, moving = False, y = 0}


update action model = case action of
  CAction a -> case C.handle_action (log "a" a) model of
    (model, eff) -> (model, Effects.map CEvent eff)
  CEvent ev ->
    let model = C.handle_event (log "ev" ev) model
    in (model, Effects.none)

app = start
  { init = (init_model, Effects.none)
  , view = view
  , update = update
  , inputs = [] }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
