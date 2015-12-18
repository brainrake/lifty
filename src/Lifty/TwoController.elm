module Lifty.TwoController where

import Maybe       as M
import Maybe.Extra as M exposing ((?))
import List        as L
import List.Extra  as L
import Array       as A exposing (Array)
import Array.Extra as A
import Set              exposing (Set, insert, remove, member, union)
import Time             exposing (Time, second)

import Lifty.Util exposing (zipiA)
import Lifty.OneController exposing (FloorId, LiftId)


type Action = CallUp FloorId
            | CallDown FloorId
            | Go LiftId FloorId
            | Approach LiftId FloorId
            | Arrive LiftId FloorId
            | Idle LiftId

type alias Lift l = { l | busy : Bool
                        , up: Bool
                        , last: FloorId
                        , dests: Set FloorId }

type alias State s l = { s | lifts : Array (Lift l)
                           , calls_up : Set FloorId
                           , calls_down : Set FloorId}


move_delay = 1 * second -- per floor
stop_delay = 1 * second

is_coming : Int -> Bool -> State s l -> Bool
is_coming floor_id up s =
  s.lifts |> A.filter (not << .busy)
          |> A.filter (\l -> (l.busy)
                          && ((not up) && l.last > floor_id && (not l.up))
                          || ((    up) && l.last < floor_id && (    l.up)))
          |> not << A.isEmpty

find_idle dest s =
  s.lifts |> zipiA |> L.filter (snd >> (not << .busy))
                   |> L.minimumBy (snd >> \l -> abs (l.last - dest))

at_end floor_id s = (floor_id == 0) || (floor_id == ((A.length s.floors) - 1))

next_in_dir src up = if up then src + 1 else src - 1

next_dest src up l s =
  let forward  = union l.dests (if up then s.calls_up else s.calls_down)
               |> Set.filter (\n -> if up then n > src else n < src)
               |> Set.toList |> L.minimum
      backward = union l.dests (if up then s.calls_up else s.calls_down)
               |> Set.filter (\n -> if up then n > src else n < src)
               |> Set.toList |> L.maximum
  in forward `M.or` backward

start lift_id l dest s =
  if l.busy then (s, Nothing) else
  if l.last == dest then (s, Just (0, Arrive lift_id dest))
  else ( { s | lifts = A.set lift_id (
           { l | busy = True, up = dest > l.last}) s.lifts }
       , Just (0, Approach lift_id (next_in_dir l.last (dest > l.last)) ) )

approach lift_id l floor_id s =
  let s' = s --{ s | lifts = A.set lift_id { l | last = floor_id } s.lifts }
  in if at_end floor_id s
     --|| next_dest src
     then (s', Just (move_delay, Arrive lift_id floor_id))
     else (s', Just (move_delay, Approach lift_id (next_in_dir floor_id l.up)))


call floor_id up s =
  let s' = if up then { s | calls_up = insert floor_id s.calls_up}
                 else { s | calls_down = insert floor_id s.calls_down}
  in if is_coming floor_id up s then (s', Nothing)
     else ( find_idle floor_id s'
            |> M.map (\(lift_id, l) -> start lift_id l floor_id s')
          ) ? (s', Nothing)

--update : Action -> State s l -> (State s l, Maybe (Time.Time, Action))
update action s = case action of
  CallUp floor_id -> call floor_id True s
  CallDown floor_id -> call floor_id False s
  Go lift_id floor_id ->
    let l = A.getUnsafe lift_id s.lifts
        l' = {l | dests = insert floor_id l.dests }
    in start lift_id l' floor_id { s | lifts = A.set lift_id l' s.lifts }
  Approach lift_id floor_id ->
    approach lift_id (A.getUnsafe lift_id s.lifts) floor_id s
  Arrive lift_id floor_id ->
    let l = A.getUnsafe lift_id s.lifts
        l' = { l | dests = remove floor_id l.dests, last = floor_id }
        s' = { s | lifts = A.set lift_id l' s.lifts
                 , calls_up = if l'.up then remove floor_id s.calls_up else s.calls_up
                 , calls_down = if not l'.up then remove floor_id s.calls_down else s.calls_down }
    in (s', Just (stop_delay, Idle lift_id))
  Idle lift_id ->
    let l = A.getUnsafe lift_id s.lifts
        l' = { l | busy = False }
        s' = { s | lifts = A.set lift_id l' s.lifts }
    in ( next_dest l'.last l'.up l s
         |> M.map (\dest -> start lift_id l' dest s)
       ) ? (s', Nothing)

