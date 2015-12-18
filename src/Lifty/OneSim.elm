module Lifty.OneSim where

import Debug
import Maybe        as M
import Maybe.Extra  as ME  exposing ((?))
import List         as L
import List.Extra   as LE
import Array        as A   exposing (Array)
import Array.Extra  as AE
import Signal       as S
import Signal.Extra as SE
import Task
import Task.Extra
import Time                exposing (Time)
import Effects      as E   exposing (Effects)
import Animation    as Ani exposing (Animation, static, retarget)

import Lifty.Util exposing (f_, zipiL, zipiA, mapiL, mapiA, delay, schedule)
import Lifty.OneController as C


type alias FloorId = Int

type alias Passenger p = { p | dest: FloorId }

type alias Lift l p = { l | pax : List (Passenger p) }

type alias State s l p =
  C.State { s | floors : Array (List (Passenger p)) } (Lift l (Passenger p))

type Action p = AddPassenger FloorId FloorId (Passenger p)
              | Action C.Action


max_queue = 4
lift_cap = 2

schedule_ = schedule Action

--  update : Action p -> State s l p
--        -> (State s l p, Maybe (Time, C.Action), Effects (Action p))
update action s = case action of
  AddPassenger src dest p ->
    let floor = AE.getUnsafe src s.floors
    in if L.length floor < max_queue
       then ( { s | floors = AE.update src (\f -> p :: f) s.floors }
            , Nothing, E.task <| Task.succeed <| Action <| C.Call src)
       else (s, Nothing, E.none)
  Action a -> case C.update (Debug.log "Action" a) s of (s', ma) -> case a of
    C.Arrive lift_id floor_id ->
      let floor = AE.getUnsafe floor_id s'.floors
          lift = AE.getUnsafe lift_id s'.lifts
          (ileaving, ipax') = lift.pax |> zipiL
                            |> L.partition (\(_, p) -> p.dest == floor_id)
          spaces = lift_cap - L.length ipax'
          irfloor = L.reverse <| zipiL floor
          (ientering, ifloor') = ( L.take spaces irfloor, L.drop spaces irfloor)
          ipax'' = L.append ipax' ientering
          pax'' =  mapiL ipax'' <| \(new, (old, p)) ->
            { p | x = retarget s.t (f_ lift_id + (f_ new) / 3) p.x }
          floor' = L.reverse (mapiL (ifloor') <| \(new, (old, p)) ->
            { p | x = retarget s.t (2 + (f_ new) / 3) p.x})
          leaving' = ileaving |> L.map (\(_, p) ->
            { p | x = retarget s.t (-3) p.x })
          s'' = { s' | floors = A.set floor_id floor' s'.floors
                     , lifts = A.set lift_id { lift | pax = pax''} s'.lifts
                     , leaving = L.append leaving' s'.leaving }
          in (s'', ma, schedule_ ma )
    C.Idle lift_id ->
      let lift = AE.getUnsafe lift_id s'.lifts
      in if L.isEmpty lift.pax
         then s'.floors |> zipiA
              |> L.filter (\(_, floor) -> not (L.isEmpty floor))
              |> L.head |> M.map (\(floor_id, _) ->
                case update (Action (C.Call floor_id)) s' of
                  (s'', ma', ef) -> (s'', ma', E.batch [schedule_ ma, ef]))
              |> M.withDefault (s', ma, schedule_ ma)
         else
           let dest = lift.pax |> L.map (\p -> p.dest)
                               |> LE.minimumBy (\d -> abs (lift.dest - d))
                               |> M.withDefault lift.dest
           in update (Action (C.Go lift_id dest)) s'
    _ -> (s', ma, schedule_ ma)
