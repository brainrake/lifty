module Lifty.OneBuilding where

import Debug
import Array        as A
import Array.Extra  as AE
import Signal       as S
import Signal.Extra as SE
import Task
import Task.Extra
import Time
import Effects      as E
import Animation    as Ani exposing (Animation, animation, static,  animate)
import StartApp

import Lifty.OneController as C
import Lifty.OneView       as V

type alias Model = { t : Time.Time, lifts : C.Model { ani : Ani.Animation }}

type Action = Action C.Action | Tick Time.Time

num_floors = 5
num_lifts = 2

init_model = { t = 0, lifts = A.repeat num_lifts { dest = 0, busy = False, ani = static 0}}

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
  , view = V.view num_floors num_lifts Action
  , update = update
  , inputs = [Time.fps 30 |> S.foldp (+) 0 |> S.map Tick] }

main = app.html

port tasks : Signal (Task.Task E.Never ())
port tasks = app.tasks

