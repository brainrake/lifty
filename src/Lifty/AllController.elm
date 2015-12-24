module Lifty.AllController where

import Debug exposing (log)
import Maybe       as M
import Maybe.Extra as M exposing ((?))
import List        as L
import List.Extra  as L
import Array       as A exposing (Array)
import Array.Extra as A
import Time             exposing (Time, second)

import Lifty.Util          exposing (f_, izipA)
import Lifty.OneController exposing (FloorId, LiftId)


type alias Passenger p = { p | src : FloorId
                             , dest : FloorId
                             , mlift : Maybe LiftId }

type alias Lift l p = { l | busy : Bool
                          , up : Bool
                          , next: FloorId
                          , pax : List (Passenger p)
                          , cap : Int }

type alias State s l f p = { s | lifts : Array (Lift l (Passenger p))
                               , floors : Array f
                               , pax : List (Passenger p)
                               , leaving : List (Passenger p)}

type Action p = AddPassenger (Passenger p)
              | Approach LiftId FloorId
              | Arrive LiftId FloorId
              | Idle LiftId

move_delay = 1 * second -- per floor
stop_delay = 1 * second

find_idle dest s =
  s.lifts |> izipA |> L.filter (snd >> (not << .busy))
                   |> L.minimumBy (snd >> \l -> abs (l.next - dest))

end up s = if up then (A.length s.floors) - 1 else 0

next_in_dir src up = if up then src + 1 else src - 1

start lift_id l dest s =
  if l.busy then (s, Nothing) else let
    l' = { l | busy = True, up = dest > l.next }
    s' = { s | lifts = A.set lift_id l' s.lifts }
  in if l.next == dest then (s', Just (0, Arrive lift_id dest))
     else (s', Just (0, Approach lift_id (next_in_dir l.next (dest > l.next))))

merge_lp lift_id l lp pax pred = let
  epax' = pax |> L.map (\p -> let
    fits p = lp |> L.all (\p' -> p.src >= p'.dest || p.dest <= p'.src)
    in_path = if l.up
              then p.dest > p.src && p.src >= l.next
              else p.dest < p.src && p.dest <= l.next
    in if pred p && fits p && in_path
       then (True, { p | mlift = Just lift_id })
       else (False, p))
  in log "merge_lp" (epax' |> L.map snd, L.append lp (epax' |> L.filter fst |> L.map snd))

merge_llp lift_id l llp pax pred = case llp of
  [] -> (pax, llp)
  lp :: llp' -> let
    (pax', lp') = merge_lp lift_id l lp pax pred
    (pax'', llp'') = merge_llp lift_id l llp' pax' pred
    in (pax'', (lp' ::llp''))

merge_lift lift_id l pax = let
  llp = A.toList <| A.initialize l.cap (\i ->
    (A.get i (A.fromList l.pax) |> M.map (\p -> [p])) ? [])
  (pax', llp') = merge_llp lift_id l llp pax (\p ->
    (p.mlift |> M.map (\i -> i == lift_id)) ? False)
  (pax'', llp'') = merge_llp lift_id l llp' pax' (\p ->
    M.isNothing p.mlift)
  in log "merge_lift" (pax'', llp'')

merge_lifts lifts pax = case lifts of
  [] -> (pax, [])
  (lift_id, l) :: lifts' -> let
    (pax', llp) = merge_lift lift_id l pax
    (pax'', llps) = merge_lifts lifts' pax'
    in (pax'', (llp :: llps))

merge s ps = { s | pax = log "merge" <| fst <| merge_lifts (izipA s.lifts) (L.append ps s.pax)}

next_stop lift_id l s = let
  ldests = l.pax |> L.map (\p -> p.dest)
  lpax = (s.pax |> L.filter (\p ->
    p.mlift |> M.map (\i -> i == lift_id) |> M.isJust))
  dests = L.append ldests (lpax |> L.map (\p -> p.src))
  in dests |> L.filter (\i -> if l.up then i >= l.next else i <= l.next)
           |> L.minimumBy (\i -> abs (i - l.next))

next_dest lift_id l s =
  l.pax |> L.map (\p -> p.dest)
        |> L.filter (\i -> if l.up then i >= l.next else i <= l.next)
        |> L.minimumBy (\i -> abs (i - l.next))


update : Action p -> State s l f p -> (State s l f p, Maybe (Time, Action p))
update action s = case action of
  AddPassenger p ->
    add_passenger p s
  Approach lift_id floor_id ->
    approach lift_id (A.getUnsafe lift_id s.lifts) floor_id s
  Arrive lift_id floor_id ->
    arrive lift_id (A.getUnsafe lift_id s.lifts) floor_id s
  Idle lift_id ->
    idle lift_id (A.getUnsafe lift_id s.lifts) s

add_passenger p s =
  case find_idle p.src s of
    Just (lift_id, l) -> let
      _ = log "found_idle" lift_id
      p' = { p | mlift = Just lift_id }
      s' = { s | pax = L.append [p'] s.pax}
      in start lift_id l p.src s'
    _ -> (merge s [p], Nothing)

approach lift_id l floor_id s = let
  l' = { l | next = floor_id, up = if floor_id == end l.up s then not l.up else l.up}
  s' = { s | lifts = A.set lift_id l' s.lifts }
  s'' = if floor_id == end l.up s then merge s' [] else s'
  should_stop = case log "next_dest" <| next_stop lift_id l s'' of
    Just d -> if d == floor_id then True else False
    _      -> False
  action = if should_stop || floor_id == end l.up s
           then Arrive lift_id floor_id
           else Approach lift_id (next_in_dir floor_id l.up)
  in (s', Just (move_delay, action))

arrive lift_id l floor_id s = let
  l' = { l | up = if floor_id == end l.up s then not l.up else l.up }
  s' = merge { s | lifts = A.set lift_id l' s.lifts } []
  (leaving, lpax) = l'.pax |> L.partition (\p -> p.dest == floor_id)
  (entering, pax') = s'.pax |> L.reverse |> L.partition (\p -> let
    lift_match = ((p.mlift |> M.map (\lift_id'-> lift_id == lift_id')) ? False)
    in p.src == floor_id && lift_match)
  l'' = { l' | pax = L.append lpax (L.reverse entering) }
  leaving' = L.append leaving s.leaving
  s'' = { s' | pax = L.reverse pax', lifts = A.set lift_id l'' s'.lifts, leaving = leaving' }
  in (s'', Just (stop_delay, Idle lift_id))

idle lift_id l s = let
  start_it s l it = (it |> M.map (\n -> start lift_id l n s)) ? (s, Nothing)
  up = if l.next == end l.up s then not l.up else
       if l.next == end (not l.up) s then l.up else l.up
  l1 = { l | busy = False, up = up }
  s1 = merge { s | lifts = A.set lift_id l1 s.lifts } []
  (s1', ma1) = start_it s1 l1 <| log "next" <| next_stop lift_id l1 s1
  _ = Debug.log "ma1" (lift_id, ma1)
  in if M.isJust ma1 then (s1', ma1)
  else if l1.next == end (not l1.up) s then (s1, Nothing) else let
  l2 = { l1 | up = not l1.up }
  s2 = merge { s1 | lifts = A.set lift_id l2 s1.lifts } []
  (s2', ma2) = start_it s2 l2 <| next_stop lift_id l2 s2
  _ = Debug.log "ma2" (lift_id, ma2)
  in if M.isJust ma2 then (s2', ma2) else (s1, Nothing)
