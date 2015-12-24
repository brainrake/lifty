module Lifty.OneSimView where

import Maybe          as M
import List           as L
import List.Extra     as L
import Array          as A   exposing (Array)
import Array.Extra    as A
import Time                  exposing (Time)
import Animation      as Ani exposing (Animation, animate, ease, retarget)
import Effects        as E   exposing (Effects, Never)

import Lifty.Util    exposing (s_, f_, izipL, imapL, anim, delay)
import Lifty.OneController as C exposing (LiftId, FloorId)
import Lifty.OneView       as V
import Lifty.OneSim        as Sim


type Action = StartAdd C.FloorId
            | FinishAdd C.FloorId
            | Tick Time

type alias Passenger p = { p | x: Animation
                             , dest : FloorId }

type alias Lift l p = { l | pax : List (Passenger p)
                          , busy : Bool
                          , dest : FloorId
                          , y : Animation }

type alias State s l p = { s | t : Time
                             , adding : Maybe C.FloorId
                             , leaving : List (Passenger p)
                             , floors : Array (List (Passenger p))
                             , lifts : Array (Lift l (Passenger p)) }


--update : Action -> State s l p -> (State s l p, Effects (Sim.Action p))
update a s = case a of
  StartAdd src -> ({ s | adding = Just src }, E.none)
  FinishAdd dest ->
    s.adding |> M.map (\src -> let
      floor = (A.getUnsafe src s.floors)
      x = anim s.t (2.3 + f_ (A.length s.lifts))  (2 + f_ (A.length s.lifts) + (f_ <| L.length floor) / 3) 500
      in Sim.update (Sim.AddPassenger src dest { x = x, dest = dest })
                    { s | adding = Nothing }
    |> \(s, ma, e) -> (s, e))
    |> M.withDefault (s, E.none)
  Tick t -> (V.update t s, E.none)


animate : Time -> State s l p -> Sim.Action p' -> State s l p -> C.Action -> State s l p
animate dt s a s' a' =
  let s'' = case a of
    Sim.Action (C.Arrive lift_id floor_id) ->
      animate_arrived lift_id floor_id s s'
    _ -> s'
  in case a' of
    C.Arrive lift_id dest ->
      let l = A.getUnsafe lift_id s.lifts
          l' = A.getUnsafe lift_id s''.lifts
          y = anim s''.t (f_ l.dest) (f_ l'.dest) dt |> ease identity
      in { s'' | lifts = A.set lift_id { l' | y = y } s''.lifts }
    _ -> s''

animate_arrived lift_id floor_id s s' = let
  l = A.getUnsafe lift_id s'.lifts
  pax' = imapL l.pax (\(i, p) ->
    { p | x = retarget s'.t (f_ lift_id + (f_ i) / 3) p.x })
  lifts' = A.set lift_id { l | pax = pax' } s'.lifts
  f = A.getUnsafe floor_id s'.floors
  f' =  L.reverse <| imapL (L.reverse f) (\(i, p) ->
    { p | x = retarget s'.t (2 + (f_ lift_id) + (f_ i) / 3) p.x})
  floors' = A.set floor_id f' s'.floors
  ldiff = L.length s'.leaving - L.length s.leaving
  (new_leaving, old_leaving) = L.splitAt ldiff s'.leaving
  new_leaving' = new_leaving |> L.map (\p ->
    { p | x = retarget s.t (-3) p.x })
  leaving' = L.append new_leaving' old_leaving
  in { s' | lifts = lifts', floors = floors', leaving = leaving' }
