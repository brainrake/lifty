module Lifty.TwoController where

import Maybe       as M
import Maybe.Extra as M exposing ((?))
import List        as L
import List.Extra  as L
import Array       as A exposing (Array)
import Array.Extra as A
import Set              exposing (Set, insert, remove, member, union)
import Time             exposing (Time, second)

import Lifty.Util exposing (izipA)
import Lifty.OneController exposing (FloorId, LiftId)


type Action = CallUp FloorId
            | CallDown FloorId
            | Go LiftId FloorId
            | Approach LiftId FloorId
            | Arrive LiftId FloorId
            | Idle LiftId

type alias Lift l = { l | busy : Bool
                        , up: Bool
                        , next: FloorId
                        , dests: Set FloorId }

type alias State s l f = { s | lifts : Array (Lift l)
                             , floors : Array f
                             , calls_up : Set FloorId
                             , calls_down : Set FloorId}


move_delay = 1 * second -- per floor
stop_delay = 1 * second

is_coming : Int -> Bool -> State s l f -> Bool
is_coming floor_id up s =
  s.lifts |> A.filter (\l -> l.busy && (
                               ((not up) && l.next >= floor_id && (not l.up)) ||
                               ((    up) && l.next <= floor_id && (    l.up))))
          |> not << A.isEmpty

find_idle dest s =
  s.lifts |> izipA |> L.filter (snd >> (not << .busy))
                   |> L.minimumBy (snd >> \l -> abs (l.next - dest))

end up s = if up then (A.length s.floors) - 1 else 0

next_in_dir src up = if up then src + 1 else src - 1

next_dest_dir next up l s =
  l.dests
  |> union (if up then s.calls_up else s.calls_down)
  |> Set.filter (\n -> if up then n >= next else n <= next)
  |> Set.toList |> (if up then L.minimum else L.maximum)

next_dest next up l s =
  (next_dest_dir next up l s) `M.or` (next_dest_dir (end up s) (not up) l s)

start lift_id l dest s =
  if l.busy then (s, Nothing) else let
  l' = { l | busy = True, up = (if dest == l.next then l.up else dest > l.next )}
  s' = { s | lifts = A.set lift_id l' s.lifts }
  a = if l.next == dest
      then Arrive lift_id dest
      else Approach lift_id (next_in_dir l.next (dest > l.next))
  in (s', Just (0, a))

update : Action -> State s l f -> (State s l f, Maybe (Time.Time, Action))
update action s = case action of
  CallUp floor_id -> call floor_id True s
  CallDown floor_id -> call floor_id False s
  Go lift_id floor_id -> let
    l = A.getUnsafe lift_id s.lifts
    l' = {l | dests = insert floor_id l.dests }
    in start lift_id l' floor_id { s | lifts = A.set lift_id l' s.lifts }
  Approach lift_id floor_id ->
    approach lift_id (A.getUnsafe lift_id s.lifts) floor_id s
  Arrive lift_id floor_id ->
    arrive lift_id (A.getUnsafe lift_id s.lifts) floor_id s
  Idle lift_id -> let
    l = A.getUnsafe lift_id s.lifts
    l' = { l | busy = False }
    s' = { s | lifts = A.set lift_id l' s.lifts }
    in ( next_dest l'.next l'.up l s
         |> M.map (\dest -> start lift_id l' dest s)
       ) ? (s', Nothing)


call floor_id up s = let
  s' = if up then { s | calls_up = insert floor_id s.calls_up}
             else { s | calls_down = insert floor_id s.calls_down}
  in if is_coming floor_id up s' then (s', Nothing)
     else ( find_idle floor_id s'
            |> M.map (\(lift_id, l) -> start lift_id l floor_id s')
          ) ? (s', Nothing)

arrive lift_id l floor_id s = let
  l' = { l | dests = remove floor_id l.dests }
  up =if floor_id == end l.up s then not l.up else
      if M.isJust <| next_dest_dir floor_id l.up l s then l.up else not l.up
  l'' = { l' | up = up }
  s' = { s | lifts = A.set lift_id l'' s.lifts
           , calls_up = if l''.up then remove floor_id s.calls_up else s.calls_up
           , calls_down = if not l''.up then remove floor_id s.calls_down else s.calls_down }
  in (s', Just (stop_delay, Idle lift_id))

approach lift_id l floor_id s = let
  s' = { s | lifts = A.set lift_id { l | next = floor_id } s.lifts }
  should_stop = case next_dest floor_id l.up l s of
    Just d -> if d == floor_id then True else False
    _      -> True
  action = if should_stop || floor_id == end l.up s
           then Arrive lift_id floor_id
           else Approach lift_id (next_in_dir floor_id l.up)
  in (s', Just (move_delay, action))
