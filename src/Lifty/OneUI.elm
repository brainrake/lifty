module Lifty.OneUI where

import Maybe         as M
import Maybe.Extra          exposing ((?))
import Array         as A   exposing (Array)
import Task                 exposing (Task)
import Time                 exposing (Time)
import Signal        as S
import Either               exposing (Either(..), elim)
import Effects       as E   exposing (Effects, Never)
import Animation            exposing (static)
import StartApp

import Lifty.Util exposing (delay)
import Lifty.OneController as C
import Lifty.OneView       as V
import Lifty.OneRender     as R


type alias Action = Either Time C.Action

type alias State = V.State (C.State { floors : Array () } {}) (C.Lift {})

num_floors = 5

init_state : State
init_state = { t = 0.0
             , floors = A.repeat num_floors ()
             , lifts = A.repeat 2 { dest = num_floors - 1
                                  , busy = False
                                  , y = static (num_floors - 1) } }

update : Action -> State -> (State, Effects Action)
update a s = a |> elim
  (\t -> (V.update t s, E.none))
  (\a -> C.update a s |> \(s', ma) ->
    ( ma |> M.map (\(dt, a') -> (V.animate dt s a' s', delay dt (Right a')))
    ) ? (s', E.none) )


app = StartApp.start
  { init = (init_state, E.none)
  , view = R.render ((<<) Right << C.Go) (Right << C.Call)
  , update = update
  , inputs = [Time.fps 30 |> S.foldp (+) 0 |> S.map Left] }

main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks
