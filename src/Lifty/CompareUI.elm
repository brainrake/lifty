module Lifty.CompareUI where

import Maybe        as M
import Maybe.Extra  as M   exposing ((?))
import List         as L
import Array        as A
import Array.Extra  as A
import Signal       as S
import Set
import Task                exposing (Task)
import Time                exposing (Time)
import Random
import Either              exposing (Either(..), elim)
import Effects      as E   exposing (Effects, Never)
import Animation    as Ani exposing (Animation, static, retarget)
import StartApp

import Lifty.Util    exposing (f_, delay, anim)
import Lifty.OneController as C1   exposing (LiftId, FloorId)
import Lifty.OneSim        as Sim1
import Lifty.OneSimView    as V1
import Lifty.OneSimRender  as R1
import Lifty.TwoController as C2
import Lifty.TwoSim        as Sim2
import Lifty.TwoSimView    as V2
import Lifty.TwoSimRender  as R2
--import Lifty.AllController as C3
--import Lifty.AllSimView    as V3
--import Lifty.AllSimRender  as R3
import Lifty.CompareRender  as R

type Action p = StartAdd FloorId
              | EndAdd FloorId
              | AddPassenger FloorId FloorId
              | Action1 (Sim1.Action p)
              | Action2 (Sim2.Action p)
              | Tick Time
              | Pax Time

init_state1 =
  { t = 0
  , floors = A.repeat 10 []
  , lifts = A.repeat 4 { dest = 0, busy = False, pax = [], y = static 0}
  , adding = Nothing
  , leaving = [] }

init_state2 =
  { t = 0
  , floors = A.repeat 10 []
  , lifts = A.repeat 4 { dests = Set.empty
                       , next = 0
                       , up = False
                       , busy = False
                       , pax = []
                       , y = static 0 }
  , calls_up = Set.empty
  , calls_down = Set.empty
  , adding = Nothing
  , leaving = [] }

init_state = { s1 = init_state1
             , s2 = init_state2
             , adding = Nothing
             , src_seed = Random.initialSeed 0
             , dest_seed = Random.initialSeed 1}


update_sim1 a s =
  (Sim1.update a s) |> \(s', ma, e) ->
    ( ma |> M.map (\(dt, a') -> (V1.animate dt s a s' a', E.map Action1 e))
    ) ? (s', E.map Action1 e)

update_sim2 a s =
  (Sim2.update a s) |> \(s', ma, e) ->
    ( ma |> M.map (\(dt, a') -> (V2.animate dt a s a' s', E.map Action2 e))
    ) ? (s', E.map Action2 e)


update a s = case a of
  StartAdd src -> ({ s | adding = Just src }, E.none)
  EndAdd dest ->
    (s.adding |> M.map (\src -> update (AddPassenger src dest) { s | adding = Nothing })
    ) ? ({ s | adding = Nothing }, E.none)
  AddPassenger src dest -> let
    floor1 = A.getUnsafe src s.s1.floors
    floor2 = A.getUnsafe src s.s2.floors
    x1 = anim s.s1.t (2.3 + f_ (A.length s.s1.lifts))
              (0 + f_ (A.length s.s1.lifts) + (f_ <| L.length floor1) / 3) 500
    p1 = { x = x1, dest = dest}
    x2 = anim s.s2.t (2.3 + f_ (A.length s.s2.lifts))
              (0 + f_ (A.length s.s2.lifts) + (f_ <| L.length floor2) / 3) 500
    p2 = { x = x2, dest = dest}
    (s1', ma1, e1) = Sim1.update (Sim1.AddPassenger src dest p1) s.s1
    (s2', ma2, e2) = Sim2.update (Sim2.AddPassenger src dest p2) s.s2
    s' = { s | s1 = s1', s2 = s2'}
    in (s', E.batch [E.map Action1 e1, E.map Action2 e2])
  Action1 a -> update_sim1 a s.s1 |> (\(s1', e) -> ({ s | s1 = s1'}, e))
  Action2 a -> update_sim2 a s.s2 |> (\(s2', e) -> ({ s | s2 = s2'}, e))
  Tick t -> let
    (s1', e1) = V1.update (V1.Tick t) (s.s1)
    (s2', e2) = V2.update (V2.Tick t) (s.s2)
    clearLeaving ls = ls |> L.filter (\l -> Ani.isDone (t-100) l.x)
    s' = { s | s1 = { s1' | leaving = s1'.leaving |> clearLeaving }
             , s2 = { s2' | leaving = s2'.leaving |> clearLeaving } }
    in (s', E.batch [E.map Action1 e1, E.map Action2 e2])
  Pax t ->
    let _ = Debug.log "Pax" t in
    if M.isJust s.adding then (s, E.none) else let
    (src, src_seed) = Random.generate (Random.int 0 9) s.src_seed
    (dest, dest_seed) = Random.generate (Random.int 0 9) s.dest_seed
    _ = Debug.log "Paxx" (src, dest)
    s' = { s | src_seed = src_seed, dest_seed = dest_seed}
    in if src /= dest then update (AddPassenger src dest) s' else (s', E.none)

app = StartApp.start
  { init = (init_state, E.none)
  , view = R.view (Action1 << Sim1.Action) StartAdd EndAdd
  , update = update
  , inputs = [ Time.fps 30 |> S.foldp (+) 0 |> S.map Tick,
               Time.fps  1 |> S.foldp (+) 0 |> S.map Pax ] }

main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks

