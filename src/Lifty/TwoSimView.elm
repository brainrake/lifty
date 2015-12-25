module Lifty.TwoSimView where

import Maybe          as M
import List           as L
import Array          as A   exposing (Array)
import Array.Extra    as A
import Time                  exposing (Time)
import Either                exposing (Either(..))
import Animation      as Ani exposing (Animation, animate, retarget, ease)
import Effects        as E   exposing (Effects, Never)

import Lifty.Util    exposing (s_, f_, izipL, anim, delay)
import Lifty.OneController        exposing (LiftId, FloorId)
import Lifty.OneSimView    as OneSimView
import Lifty.TwoController as C
import Lifty.TwoView       as V
import Lifty.TwoSim        as Sim


type alias Passenger p = { p | x: Animation }

type Action = StartAdd FloorId
            | FinishAdd FloorId
            | Tick Time


type alias State s l p = Sim.State { s | t : Time
                                       , adding : Maybe FloorId
                                       , floors : Array (List (Passenger p))
                                       , lifts : Array l } l (Passenger p)


--update : Action -> State s l p -> (State s l p, Effects (Sim.Action (Passenger p)))
update a s = case a of
  StartAdd src -> ({ s | adding = Just src }, E.none)
  FinishAdd dest ->
    s.adding
    |> M.map (\src ->
      let floor = (A.getUnsafe src s.floors)
          x = anim s.t (2.3 + f_ (A.length s.lifts)) (2 + (f_ <| L.length floor) / 3) 500
      in Sim.update (Sim.AddPassenger src dest { x = x, dest = dest })
                    { s | adding = Nothing }
    |> \(s, ma, e) -> (s, e))
    |> M.withDefault (s, E.none)
  Tick t -> (V.update t s, E.none)


-- animate : Time -> Sim.Action p -> State s l p -> C.Action -> State s l p -> State s l p
animate s a s' ma =
  let s'' = case a of
    Sim.Action (C.Approach lift_id dest) ->
      let l' = A.getUnsafe lift_id s'.lifts
          y = anim (s'.t) (Ani.animate s'.t l'.y) (f_ dest) 1000 |> ease identity
      in { s' | lifts = A.set lift_id { l' | y = y } s'.lifts }
    Sim.Action (C.Arrive lift_id floor_id) ->
      OneSimView.animate_arrived lift_id floor_id s s'
    _ -> s'
  in case ma of
    Just (dt, C.Arrive lift_id dest) ->
      let l' = A.getUnsafe lift_id s''.lifts
          y = anim (s''.t) (Ani.animate s'.t l'.y) (f_ dest) dt |> ease identity
      in { s'' | lifts = A.set lift_id { l' | y = y } s''.lifts }
    _ -> s''
