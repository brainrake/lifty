module Lifty.TwoSim where

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
import Lifty.OneController      exposing (FloorId, LiftId)
import Lifty.TwoController as C


type alias FloorId = Int

type alias Passenger p = { p | dest: FloorId }

type alias Lift l p = { l | pax : List (Passenger p) }

type alias State s l p = { s | floors : Array (List (Passenger p))
                             , lifts : Array (Lift l p)}

type Action p = AddPassenger FloorId FloorId (Passenger p)
              | Action C.Action


max_queue = 4
lift_cap = 2

schedule_ = schedule Action

--  update : Action p -> State s l p
--        -> (State s l p, Maybe (Time, C.Action), Effects (Action p))
update action s = case action of
  AddPassenger src dest p -> let
    floor = A.getUnsafe src s.floors
    in if L.length floor < max_queue
       then ( { s | floors = A.update src (\f -> p :: f) s.floors }
            , Nothing, E.task <| Task.succeed <| Action <|
                (if dest > src then C.CallUp src else C.CallDown src))
       else (s, Nothing, E.none)
  Action a -> case C.update (Debug.log "Action" a) s of (s', ma) -> case a of
    C.Arrive lift_id floor_id -> let
     (s'', eff) = arrive lift_id floor_id s'
     in (s'', ma, E.batch [schedule_ ma, eff])
    _ -> (s', ma, schedule_ ma)

arrive lift_id floor_id s = let
  floor = A.getUnsafe floor_id s.floors
  lift = A.getUnsafe lift_id s.lifts
  (ileaving, ipax) = lift.pax |> izipL
                   |> L.partition (\(_, p) -> p.dest == floor_id)
  spaces = lift_cap - L.length ipax
  ifloor = L.reverse <| izipL floor
  (ientering, ifloor') = ( L.take spaces ifloor, L.drop spaces ifloor)
  ipax' = L.append ipax ientering
  pax' =  imapL ipax' <| \(new, (old, p)) ->
    { p | x = retarget s.t (f_ lift_id + (f_ new) / 3) p.x }
  floor' = L.reverse (imapL (ifloor') <| \(new, (old, p)) ->
    { p | x = retarget s.t (2 + (f_ new) / 3) p.x})
  leaving' = ileaving |> L.map (\(_, p) ->
    { p | x = retarget s.t (-3) p.x })
  s' = { s | floors = A.set floor_id floor' s.floors
           , lifts = A.set lift_id { lift | pax = pax'} s.lifts
           , leaving = L.append leaving' s.leaving }
  eff = E.batch (ientering |> L.map (\(_, p) ->
    schedule_ (Just (0, C.Go lift_id p.dest))))
  in (s', eff)

--              in
