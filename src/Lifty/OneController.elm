module Lifty.OneController where

import Maybe       as M
import Maybe.Extra as M exposing ((?))
import List        as L
import List.Extra  as L
import Array       as A exposing (Array)
import Array.Extra as A
import Time             exposing (Time, second)

import Lifty.Util       exposing (f_, zipiA)


type alias FloorId = Int
type alias LiftId = Int

type Action = Call FloorId
            | Go LiftId FloorId
            | Arrive LiftId FloorId
            | Idle LiftId

type alias Lift l = { l | busy : Bool
                        , dest: FloorId }

type alias State s l = { s | lifts : Array (Lift l) }


move_delay = 1 * second -- per floor
stop_delay = 1 * second

--move : LiftId -> Lift l -> FloorId -> State -> (State s l, Maybe (Time, Action))
move lift_id l dest s =
  ( { s | lifts = A.set lift_id ({ l | dest = dest, busy = True}) s.lifts }
  , Just ( move_delay * (toFloat <| abs <| l.dest - dest)
         , Arrive lift_id dest ) )

update : Action -> State s l -> (State s l, Maybe (Time, Action))
update action s = case action of
  Call dest -> -- send the nearest idle lift
    ( s.lifts |> zipiA
      |> L.filter (snd >> (not << .busy))
      |> L.minimumBy (snd >> \l -> abs (l.dest - dest))
      |> M.map (\(lift_id, l) -> move lift_id l dest s)
    ) ? (s, Nothing)
  Go lift_id floor_id ->
    ( A.get lift_id s.lifts
      |> flip M.andThen (\l -> if not l.busy then Just l else Nothing)
      |> M.map (\l -> move lift_id l floor_id s)
    ) ? (s, Nothing)
  Arrive i to ->
    (s, Just (stop_delay, Idle i))
  Idle lift_id ->
    let s' = { s | lifts = A.update lift_id (\l -> { l | busy = False }) s.lifts }
    in (s', Nothing)
