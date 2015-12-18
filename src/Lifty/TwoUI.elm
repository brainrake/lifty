module Lifty.TwoSim where

import Debug
import Array        as A  exposing (Array)
import Array.Extra  as AE
import Signal       as S
import Signal.Extra as SE
import Task
import Task.Extra
import Time
import Effects      as E
import Animation    as Ani exposing (Animation, animation, static, ease)
import Html
import StartApp

import Lifty.TwoController as C
import Lifty.TwoView       as V

type alias Model = { t : Time.Time
                   , floors : Array ()
                   , lifts : C.Model { ani : Animation } }

type Action = Action C.Action
            | Tick Time.Time

init_model : Model
init_model = { t = 0
             , floors = A.repeat 5 ()
             , lifts = A.repeat 2 { dest = 0, busy = False, up = False, ani = static 0 } }

delay t act = E.task <| Task.Extra.delay t <| Task.succeed <| Action act

update : Action -> Model -> (Model, E.Effects Action)
update action model = case action of
  Action a ->
    case C.update (Debug.log "action" a) model.lifts of
      (lifts', Just (dt, act)) -> case act of
        C.Arrive i to ->
          let l = AE.getUnsafe i model.lifts
              l' = AE.getUnsafe i lifts'
              ani = animation model.t |> Ani.from (toFloat l.dest) |> Ani.to (toFloat l'.dest) |> Ani.duration dt
              lifts'' = A.set i { l' | ani = ani} lifts'
          in ({ model | lifts = lifts'' }, delay dt act)
        _ -> ({ model | lifts = lifts' }, delay dt act)
      (lifts', Nothing) -> ({ model | lifts = lifts' }, E.none)
  Tick t ->
    ({ model | t = t }, E.none)

app = StartApp.start
  { init = (init_model, E.none)
  , view = V.view Action
  , update = update
  , inputs = [Time.fps 30 |> S.foldp (+) 0 |> S.map Tick] }

main = app.html

port tasks : Signal (Task.Task E.Never ())
port tasks = app.tasks

