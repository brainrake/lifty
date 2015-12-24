module Lifty.OneSim where

import Debug
import Maybe        as M
import Maybe.Extra  as M   exposing ((?))
import List         as L
import List.Extra   as L
import Array        as A   exposing (Array)
import Array.Extra  as A
import Signal       as S
import Signal.Extra as S
import Task
import Task.Extra
import Time                exposing (Time)
import Effects      as E   exposing (Effects)
import Animation    as Ani exposing (Animation, static, retarget)

import Lifty.Util    exposing (f_, izipL, izipA, imapL, schedule)
import Lifty.OneController as C exposing (LiftId)


type alias FloorId = Int

type alias Passenger p = { p | dest: FloorId }

type alias Lift l p = { l | pax : List (Passenger p)
                          , busy : Bool
                          , dest : FloorId }

type alias State s l p = { s | floors : Array (List (Passenger p))
                             , lifts : Array (Lift l p)
                             , leaving : List (Passenger p)}

type Action p = AddPassenger FloorId FloorId (Passenger p)
              | Action C.Action


max_queue = 4
lift_cap = 2

schedule_ = schedule Action

update : Action p -> State s l p
      -> (State s l p, Maybe (Time, C.Action), Effects (Action p))
update action s = case action of
  AddPassenger src dest p -> let
    floor = A.getUnsafe src s.floors
    in if L.length floor < max_queue
       then ( { s | floors = A.update src (\f -> p :: f) s.floors }
            , Nothing, E.task <| Task.succeed <| Action <| C.Call src)
       else (s, Nothing, E.none)
  Action a -> case C.update (Debug.log "Action" a) s of (s', ma) -> case a of
    C.Arrive lift_id floor_id ->  (arrive lift_id floor_id s', ma, schedule_ ma)
    C.Idle lift_id -> idle lift_id ma s'
    _ -> (s', ma, schedule_ ma)

arrive lift_id floor_id s = let
  floor = A.getUnsafe floor_id s.floors
  lift = A.getUnsafe lift_id s.lifts
  (leaving, pax) = lift.pax |> L.partition (\p -> p.dest == floor_id)
  spaces = lift_cap - L.length pax
  rfloor = L.reverse floor
  (entering, rfloor') = ( L.take spaces rfloor, L.drop spaces rfloor)
  pax' = L.append pax entering
  floor' = L.reverse rfloor'
  in { s | floors = A.set floor_id floor' s.floors
         , lifts = A.set lift_id { lift | pax = pax'} s.lifts
         , leaving = L.append leaving s.leaving }

idle lift_id ma s = let
  l = A.getUnsafe lift_id s.lifts
  in if L.isEmpty l.pax
     then ( izipA s.floors
            |> L.filter (\(_, floor) -> not (L.isEmpty floor))
            |> L.head |> M.map (\(floor_id, _) ->
              case update (Action (C.Call floor_id)) s of
                (s', ma', ef) -> (s', ma', E.batch [schedule_ ma, ef]))
          ) ? (s, ma, schedule_ ma)
     else let dest = l.pax |> L.map .dest
                           |> L.minimumBy (\d -> abs (l.dest - dest))
                           |> M.withDefault l.dest
              in update (Action (C.Go lift_id dest)) s
