module Lifty.AllSimUI where

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
import Lifty.OneController as C1
import Lifty.OneSim        as Sim1
import Lifty.OneSimView    as V1
import Lifty.OneSimRender  as R1
import Lifty.TwoController as C2
import Lifty.TwoSim        as Sim2
import Lifty.TwoSimView    as V2
import Lifty.TwoSimRender  as R2
--import Lifty.ManyController as C2
--import Lifty.ManySim        as Sim2
--import Lifty.ManySimView    as V2
--import Lifty.ManySimRender  as R2


type alias Action p = Either (V.Action p) (Sim.Action p)


init_state =
  { t = 0
  , floors = A.repeat 5 []
  , lifts = A.repeat 2 { dest = 0, busy = False, pax = [], y = static 0}
  , adding = Nothing
  , leaving = [] }

--update : Action p -> V.State s l p -> (V.State s l p, Effects (Action p))
update a s = a |> elim
  (\a -> V.update a s |> \(s', e) -> (s', E.map Right e))
  (\a -> Sim.update a s |> \(s', ma, e) ->
    ( ma |> M.map (\(dt, a') -> (V.animate s s' dt a', E.map Right e))
    ) ? (s', E.map Right e) )

app = StartApp.start
  { init = (init_state, E.none)
  , view = R.view (Right << Sim.Action)
                  C.Go
                  C.Call
                  (Left << V.StartAdd)
                  (Left << V.FinishAdd)
  , update = update
  , inputs = [Time.fps 30 |> S.foldp (+) 0 |> S.map (Left << V.Tick)] }

main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks

