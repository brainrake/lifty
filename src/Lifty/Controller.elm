module Lifty.SimpleController where

import Time exposing (second)
import Array exposing (Array, repeat)
import Effects exposing (Effects, task)
--import Task exposing (delay)

import Lifty.Base exposing (FloorId, Passenger)

-- debug
import Debug exposing (watch)
import Html exposing (text)


type alias LiftId = Int
type alias Lift = { from : FloorId, to : FloorId, pax : List Passenger }
type alias Model = Array Lift

type Action = Call FloorId | Go LiftId FloorId

type Event  = Arrive LiftId FloorId | Standby Lift


max_pax = 4
move_time = 1 * second -- per floor
stop_time = 1 * second


init : Int -> Model
init num_lifts =
  repeat num_lifts { from = 0, to = 0, pax = [] }

update : Action -> Model -> (Model, Effects Event)
update action model = case action of
  Call to    -> (model, Effects.none)
  Go lift to -> (model, Effects.none)


--view : Signal.Adress Action -> Model -> Html

main = text <| toString <| (watch "Lifty.Controller.Model" (init 3))
