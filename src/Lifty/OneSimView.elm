module Lifty.OneSimView where

import Maybe          as M
import List           as L
import Array          as A   exposing (Array)
import Array.Extra    as A
import Time                  exposing (Time)
import Animation      as Ani exposing (Animation, animate, retarget)
import Effects        as E   exposing (Effects, Never)

import Lifty.Util    exposing (s_, f_, zipiL, mapiA, anim, delay)
import Lifty.OneController as C
import Lifty.OneView       as V
import Lifty.OneSim        as Sim

type alias Passenger p = { p | x: Animation }

type Action = StartAdd C.FloorId
            | FinishAdd C.FloorId
            | Tick Time

type alias State s l p = V.State { s | adding : Maybe C.FloorId
                                     , leaving : List (Passenger p) }
                                 l
                                 (List (Passenger p))


--update : Action p -> State s l p -> (State s l p, Effects Action)
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
animate s s' dt a' = case a' of
  C.Arrive lift_id dest ->
    let l = A.getUnsafe lift_id s.lifts
        l' = A.getUnsafe lift_id s'.lifts
        y = anim s'.t (f_ l.dest) (f_ l'.dest) dt
        (ileaving, ipax') = l.pax |> zipiL
                          |> L.partition (\(_, p) -> p.dest == dest)
        floor = A.getUnsafe dest s'.floors
        floor' = floor |> L.map (\p -> {p | x = p.x})
    in { s' | floors = A.set dest floor' s'.floors
            , lifts = A.set lift_id { l' | y = y } s'.lifts }
  _ -> s'
