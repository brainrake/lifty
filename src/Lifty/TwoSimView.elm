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
import Lifty.TwoController as C
import Lifty.TwoView       as V
import Lifty.TwoSim        as Sim


type alias Passenger p = { p | x: Animation }

type Action = StartAdd FloorId
            | FinishAdd FloorId
            | Tick Time


type alias State s l p = { s | t : Time
                              , adding : Maybe FloorId
                              , leaving : List (Passenger p)
                              , lifts : Array l }


-- update : Action -> State s l p -> (State s l p, Effects (Sim.Action p))
update a s = case a of
  StartAdd src -> ({ s | adding = Just src }, E.none)
  FinishAdd dest ->
    s.adding
    |> M.map (\src ->
      let floor = (A.getUnsafe src s.floors)
          x = anim s.t 4.3  (2 + (f_ <| L.length floor) / 3) 500
      in Sim.update (Sim.AddPassenger src dest { x = x, dest = dest })
                    { s | adding = Nothing }
    |> \(s, ma, e) -> (s, e))
    |> M.withDefault (s, E.none)
  Tick t -> (V.update t s, E.none)


--animate : State s l p -> State s l p -> Time -> C.Action -> State s l p
animate dt a s a' s' =
  let s'' = case a of
    Sim.Action (C.Approach lift_id dest) ->
      let l' = A.getUnsafe lift_id s'.lifts
          y = anim (s'.t) (Ani.animate s'.t l'.y) (f_ dest) dt |> ease identity
      in { s' | lifts = A.set lift_id { l' | y = y } s'.lifts }
    _ -> s'
  in case a' of
    C.Arrive lift_id dest ->
      let l' = A.getUnsafe lift_id s''.lifts
          y = anim (s''.t) (Ani.animate s'.t l'.y) (f_ dest) dt |> ease identity
      in { s'' | lifts = A.set lift_id { l' | y = y } s''.lifts }
    _ -> s''
