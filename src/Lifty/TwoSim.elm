module Lifty.TwoSim where

import Maybe        as M
import Maybe.Extra  as M   exposing ((?))
import List         as L
import List.Extra   as L
import Array        as A   exposing (Array)
import Array.Extra  as A
import Set
import Signal       as S
import Signal.Extra as S
import Task
import Task.Extra
import Time                exposing (Time)
import Effects      as E   exposing (Effects)
import Animation    as Ani exposing (Animation, static, retarget)

import Lifty.Util    exposing (f_, izipL, izipA, imapL, imapA, partitionUpto, schedule)
import Lifty.OneController      exposing (FloorId, LiftId)
import Lifty.TwoController as C


type alias FloorId = Int

type alias Passenger p = { p | dest: FloorId }

type alias Lift l p = { l | pax : List (Passenger p) }

type alias State s l p = C.State { s | leaving : List (Passenger p)
                                     , max_queue : Int
                                     , lift_cap : Int }
                                 (Lift l p)
                                 (List (Passenger p))

type Action p = AddPassenger FloorId FloorId (Passenger p)
              | Action C.Action


schedule_ = schedule Action

update : Action p -> State s l p
       -> (State s l p, Maybe (Time, C.Action), Effects (Action p))
update action s = case action of
  AddPassenger src dest p -> let
    floor = A.getUnsafe src s.floors
    (s', ma) = if L.length floor < s.max_queue
               then ( { s | floors = A.update src (\f -> p :: f) s.floors }
                    , Just (if dest > src then C.CallUp src else C.CallDown src))
               else (s, Nothing)
    in (ma |> M.map (\a -> let (s'', ma') = C.update a s'
                           in (s'', ma', schedule_ ma'))
       ) ? (s, Nothing, E.none)
  Action a -> case C.update a s of (s', ma) -> case a of
    C.Arrive lift_id floor_id ->
     let (s'', eff) = arrive lift_id floor_id s'
     in (s'', ma, E.batch [schedule_ ma, eff])
    C.Idle lift_id ->
     let (s'', eff) = idle s'
     in (s'', ma, E.batch [schedule_ ma, eff])
    _ -> (s', ma, schedule_ ma)

arrive lift_id floor_id s = let
  floor = A.getUnsafe floor_id s.floors
  lift = A.getUnsafe lift_id s.lifts
  (leaving, pax) = lift.pax |> L.partition (\p -> p.dest == floor_id)
  spaces = s.lift_cap - L.length pax
  rfloor = L.reverse floor
  pred = \p -> lift.up == (p.dest > floor_id)
  (entering, rfloor') = partitionUpto spaces pred rfloor
  pax' = L.append pax entering
  floor' = L.reverse rfloor'
  s' = { s | floors = A.set floor_id floor' s.floors
           , lifts = A.set lift_id { lift | pax = pax'} s.lifts
           , leaving = L.append leaving s.leaving }
  eff = E.batch (entering |> L.map (\p ->
    schedule_ (Just (0, C.Go lift_id p.dest))))
  in (s', eff)

idle s = let
  effss = imapA s.floors <| \(floor_id, floor) ->
    L.concat (floor |> L.map (\p -> let
      call = if p.dest > floor_id then C.CallUp else C.CallDown
      nexts = Set.fromList (A.toList (s.lifts |> A.map .next))
      calls = if p.dest > floor_id then s.calls_up else s.calls_down
      called = Set.member floor_id calls
      is_here = Set.member floor_id nexts
      in if called || is_here then []
         else [schedule_ (Just (0, (call floor_id)))] ))
  in (s, E.batch (L.concat effss))
