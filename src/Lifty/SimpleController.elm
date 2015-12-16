module Lifty.SimpleController where

import Debug
import Maybe as M
import Array as A exposing (Array)
import Array.Extra as AE
import List as L
import List.Extra as LE
import Time exposing (second)
import Task
import Task.Extra exposing (delay)
import Effects exposing (Effects, task)

import Lifty.Base exposing (FloorId)


type alias LiftId = Int
type alias Lift a = { a | dest : FloorId, moving : Bool }
type alias Model a = Array (Lift a)

type Action = Call FloorId
            | Go LiftId FloorId

type Event  = Arrive LiftId FloorId
            | Standby


move_time = 1 * second -- per floor
stop_time = 1 * second


move_to model i l to =
  let time = move_time * (toFloat <| abs <| l.dest - to)
      eff = Effects.task <| delay time <| Task.succeed <| Arrive i to
      model = A.set i ({ l | dest = to, moving = True}) model
  in (model, eff)

handle_action : Action -> Model a -> (Model a, Effects Event)
handle_action action model = case action of
  Call to -> A.toList model -- find the nearest empty lift
    |> L.indexedMap (,)
    |> L.filter (\(i, l) -> not l.moving)
    |> LE.minimumBy (\(i, l) -> abs <| l.dest - to)
    |> M.map (\(i, l) -> move_to model i l to)
    |> M.withDefault (model, Effects.none)
  Go i to ->
    let l = AE.getUnsafe i model
    in  if not l.moving
        then move_to model i l to
        else (model, Effects.none)


handle_event : Event -> Model a -> Model a
handle_event event model = case event of
  Arrive i to ->
    let l = AE.getUnsafe i model
    in A.set i { l | moving = False } model
  Standby -> model
