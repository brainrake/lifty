module Lifty.OneSimUI where

import Maybe        as M
import Maybe.Extra  as M   exposing ((?))
import Array        as A
import Signal       as S
import Task                exposing (Task)
import Time                exposing (Time)
import Either              exposing (Either(..), elim)
import Effects      as E   exposing (Effects, Never)
import Animation    as Ani exposing (Animation, static, retarget)
import StartApp

import Lifty.Util    exposing (delay)
import Lifty.OneController as C
import Lifty.OneSim        as Sim
import Lifty.OneSimView    as V
import Lifty.OneSimRender  as R


type alias Passenger = V.Passenger {}

type alias Action = Either (V.Action) (Sim.Action Passenger)

type alias State = V.State (Sim.State (C.State {} {}) {} {}) {} {}

num_floors = 5

init_state : State
init_state =
  { t = 0
  , floors = A.repeat num_floors []
  , adding = Nothing
  , leaving = []
  , max_queue = 4
  , lift_cap = 2
  , lifts = A.repeat 2 { dest = num_floors - 1
                       , busy = False
                       , pax = []
                       , y = static (num_floors - 1) } }

--update : Action -> State -> (State, Effects (Action))
update a s = a |> elim
  (\a -> V.update a s |> \(s', e) -> (s', E.map Right e))
  (\a -> Sim.update a s |> \(s', ma, e) ->
    ( ma |> M.map (\(dt, a') -> (V.animate dt s a s' a', E.map Right e))
    ) ? (s', E.map Right e) )

app = StartApp.start
  { init = (init_state, E.none)
  , view = R.render ((<<) Right << ((<<) Sim.Action << C.Go))
                    (Right << (Sim.Action << C.Call))
                    (Left << V.StartAdd)
                    (Left << V.FinishAdd)
  , update = update
  , inputs = [Time.fps 30 |> S.foldp (+) 0 |> S.map (Left << V.Tick)] }

main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks

