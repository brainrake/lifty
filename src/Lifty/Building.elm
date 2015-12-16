module Lifty.Building where

import Effects exposing (Effects)
import Array exposing (Array, initialize)

import Lifty.Passenger exposing (Passenger)
import Lifty.Controller as C


type alias Floor = List Passenger
type alias Model = Array Floor

type Action = Queue FloorId Passenger

init : Int        -> (Model, Effects Action)
init = num_floors ->
  ( initialize num_floors \id -> { id = id, pax = [] }
  , Effects.none)


update : Model ->  Action -> (Model, Effects Action)
update model action = case action of
  Queue FloorId Passenger ->


-- action -> state -> /-> controller ->\ -> output (lift pos)
--                    \-------<--------/
