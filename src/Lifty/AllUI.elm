module Lifty.AllUI where

import Debug
import Maybe        as M
import Maybe.Extra         exposing ((?))
import Array        as A   exposing (Array)
import Set                 exposing (Set)
import Signal       as S
import Either              exposing (Either(..), elim)
import Task                exposing (Task)
import Task.Extra
import Time                exposing (Time)
import Effects      as E   exposing (Effects, Never)
import Animation    as Ani exposing (Animation, static)
import AnimationFrame
import StartApp

import Lifty.Util    exposing (delay, schedule)
import Lifty.AllController as C
import Lifty.AllView       as V
import Lifty.AllRender     as R


type alias Action = Either V.Action (C.Action (V.Passenger {}))

type alias State = V.State (C.State {} {} () (V.Passenger {})) (C.Lift {} {}) (C.Passenger ())

--init_state : State
init_state = { t = 0
             , floors = A.repeat 5 ()
             , pax = []
             , adding = Nothing
             , leaving = []
             , lift_cap = 2
             , lifts = A.repeat 1 { busy = False
                                  , up = False
                                  , next = 0
                                  , pax = []
                                  , y = static 0 } }

schedule_ = schedule Right

--update : Action -> State -> (State, Effects Action)
update a s = a |> elim
  (\a -> V.update a s |> \(s', e) -> (s', E.map Right e))
  (\a -> C.update (Debug.log "a" a) s |> \(s', ma) ->
    (V.animate s a s' ma, schedule_ ma))

app = StartApp.start
  { init = (init_state, E.none)
  , view = R.view (Left << V.StartAdd)
                  (Left << V.FinishAdd)
  , update = update
  , inputs = [AnimationFrame.frame |> S.foldp (+) 0 |> S.map (Left << V.Tick)] }

main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks












