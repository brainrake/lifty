module Lifty.TwoController where

import Maybe       as M
import Array       as A  exposing (Array)
import List        as L
import List.Extra  as LE
import Set               exposing (Set)
import Time


type alias FloorId = Int
type alias LiftId = Int

type Action = CallUp FloorId
            | CallDown FloorId
            | Go LiftId FloorId
            | Pass LiftId FloorId
            | Arrive LiftId FloorId
            | Idle LiftId

type alias Lift l = { l | busy : Bool
                        , up: Bool
                        , dests: Set FloorId }

type alias State s l = { s | lifts : Array (Lift l)
                           , calls_up : Set FloorId
                           , calls_down : Set FloorId}



move_time = 1 * Time.second -- per floor
stop_time = 1 * Time.second

move_to model i l to =
  let time = move_time * (toFloat <| abs <| l.dest - to)
      model = A.set i ({ l | dest = to, busy = True}) model
  in (model, Just (time, Arrive i to))


      --A.toList model
      --|> L.indexedMap (,)
      --|> L.filter (\(_, l) -> not l.busy )--&& l.dest /= to)
      --|> LE.minimumBy (\(_, l) -> abs <| l.dest - to)
      --|> M.map (\(i, l) -> move_to model i l to)


update : Action -> Model a -> (Model a, Maybe (Time.Time, Action))
update action model =
  M.withDefault (model, Nothing) <| case action of
    CallUp floor_id ->
      Just ({ model | calls_up = Set.insert floor_id model.calls_up}, Nothing)
    CallDown floor_id -> Nothing
    Go lift_id floor_id ->
      A.get lift_id model
      |> (flip M.andThen) (\l -> if not l.busy then Just l else Nothing)
      |> M.map (\l -> move_to model lift_id l floor_id)
    Pass lift_id floor_id -> Nothing
    Arrive i to ->
      Just (model, Just (stop_time, Idle i))
    Idle i ->
      A.get i model
      |> M.map (\l -> (A.set i { l | busy = False } model, Nothing))
