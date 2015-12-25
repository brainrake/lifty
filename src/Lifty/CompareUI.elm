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
import AnimationFrame

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
              | AddRandom Time
              | ToggleRandom Bool
              | Fill
              | Nop

num_floors = 10
num_lifts = 4
max_queue = 16
lift_cap = 4

init_state1 =
  { t = 0
  , floors = A.repeat num_floors []
  , adding = Nothing
  , leaving = []
  , max_queue = max_queue
  , lift_cap = lift_cap
  , lifts = A.repeat num_lifts { dest = num_floors - 1
                               , busy = False
                               , pax = []
                               , y = static (num_floors - 1) } }

init_state2 =
  { t = 0
  , floors = A.repeat num_floors []
  , calls_up = Set.empty
  , calls_down = Set.empty
  , adding = Nothing
  , leaving = []
  , max_queue = max_queue
  , lift_cap = lift_cap
  , lifts = A.repeat num_lifts { dests = Set.empty
                               , next = num_floors - 1
                               , up = False
                               , busy = False
                               , pax = []
                               , y = static (num_floors - 1) } }

init_state = { s1 = init_state1
             , s2 = init_state2
             , adding = Nothing
             , seed = Random.initialSeed 0
             , random_enabled = True }


update_sim1 a s =
  (Sim1.update a s) |> \(s', ma, e) ->
    ( ma |> M.map (\(dt, a') -> (V1.animate dt s a s' a', E.map Action1 e))
    ) ? (s', E.map Action1 e)

update_sim2 a s =
  (Sim2.update a s) |> \(s', ma, e) ->
    ( ma |> M.map (\(dt, a') -> (V2.animate dt a s a' s', E.map Action2 e))
    ) ? (s', E.map Action2 e)

gen_dest src seed =
  let (dest, seed') = Random.generate (Random.int 0 (num_floors - 1)) seed
  in if src == dest then gen_dest src seed'
                    else (dest, seed')

gen_srcdest seed = let
  (src, seed') = Random.generate (Random.int 0 (num_floors - 1)) seed
  (dest, seed'') = gen_dest src seed'
  in ((src, dest), seed'')

update a s = case a of
  StartAdd src -> ({ s | adding = Just src }, E.none)
  EndAdd dest ->
    (s.adding |> M.map (\src -> update (AddPassenger src dest) { s | adding = Nothing })
    ) ? ({ s | adding = Nothing }, E.none)
  AddPassenger src dest -> let
    floor1 = A.getUnsafe src s.s1.floors
    floor2 = A.getUnsafe src s.s2.floors
    x1 = anim s.s1.t (3.3 + f_ (A.length s.s1.lifts))
              (0 + f_ (A.length s.s1.lifts) + (f_ <| L.length floor1) / 5) 500
    p1 = { x = x1, dest = dest}
    x2 = anim s.s2.t (3.3 + f_ (A.length s.s2.lifts))
              (0 + f_ (A.length s.s2.lifts) + (f_ <| L.length floor2) / 5) 500
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
  AddRandom t ->
    if M.isJust s.adding || not s.random_enabled then (s, E.none) else let
    ((src, dest), seed') = gen_srcdest s.seed
    in update (AddPassenger src dest) { s | seed = seed' }
  ToggleRandom enabled ->
    let _ = Debug.log "ToggleRandom" enabled in
    ({ s | random_enabled = enabled }, E.none)
  Fill -> let
    gen_floor floor_id p_id (acts, seed) =
      if p_id < 0 then (acts, seed)
      else let (dest, seed') = gen_dest floor_id seed
               act = AddPassenger floor_id dest
               (acts', seed'') = gen_floor floor_id (p_id - 1) (acts, seed')
           in (act :: acts', seed'')
    gen_all floor_id (acts, seed) =
      if floor_id < 0 then (acts, seed)
      else let (acts', seed') = gen_floor floor_id (max_queue - 1) (acts, seed)
           in gen_all (floor_id - 1) (acts', seed')
    (acts, seed') = gen_all (num_floors - 1) ([], s.seed)
    upd a (s, e) = let
      (s', e') = update a s
      in (s', E.batch [e, e'])
    in L.foldl upd ({ s | seed = seed' }, E.none) acts
  Nop -> (s, E.none)


app = StartApp.start
  { init = (init_state, E.none)
  , view = R.view StartAdd EndAdd ToggleRandom Fill Nop
  , update = update
  , inputs = [ AnimationFrame.frame |> S.foldp (+) 0 |> S.map Tick,
               Time.fps  1 |> S.foldp (+) 0 |> S.map AddRandom ] }

main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks

