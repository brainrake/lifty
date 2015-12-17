module Lifty.OneController where

import Maybe       as M
import Array       as A  exposing (Array)
import List        as L
import List.Extra  as LE
import Time


type alias FloorId = Int
type alias LiftId = Int
type alias Lift a = { a | busy : Bool, dest: FloorId }
type alias Model a = Array (Lift a)

type Action = Call FloorId
            | Go LiftId FloorId
            | Arrive LiftId FloorId
            | Idle LiftId


move_time = 1 * Time.second -- per floor
stop_time = 1 * Time.second

move_to model i l to =
  let time = move_time * (toFloat <| abs <| l.dest - to)
      model = A.set i ({ l | dest = to, busy = True}) model
  in (model, Just (time, Arrive i to))

update : Action -> Model a -> (Model a, Maybe (Time.Time, Action))
update action model =
  M.withDefault (model, Nothing) <| case action of
    Call to -> -- send the nearest idle lift
      A.toList model
      |> L.indexedMap (,)
      |> L.filter (\(_, l) -> not l.busy )--&& l.dest /= to)
      |> LE.minimumBy (\(_, l) -> abs <| l.dest - to)
      |> M.map (\(i, l) -> move_to model i l to)
    Go lift_id floor_id ->
      A.get lift_id model
      |> (flip M.andThen) (\l -> if not l.busy then Just l else Nothing)
      |> M.map (\l -> move_to model lift_id l floor_id)
    Arrive i to ->
      Just (model, Just (stop_time, Idle i))
    Idle i ->
      A.get i model
      |> M.map (\l -> (A.set i { l | busy = False } model, Nothing))
