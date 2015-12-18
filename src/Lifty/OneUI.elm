module Lifty.OneUI where

import Maybe         as M
import Maybe.Extra          exposing ((?))
import Array         as A
import Task                 exposing (Task)
import Time                 exposing (Time)
import Signal        as S
import Either               exposing (Either(..), elim)
import Effects       as E   exposing (Effects, Never)
import Animation     as Ani exposing (Animation, static)
import StartApp

import Lifty.Util exposing (delay)
import Lifty.OneController as C
import Lifty.OneView       as V
import Lifty.OneRender     as R


type alias Action = Either Time C.Action

init_state = { t = 0.0
             , floors = A.repeat 5 ()
             , lifts = A.repeat 2 { dest = 0, busy = False, y = static 0 } }

update : Action -> V.State s l a -> (V.State s l a, Effects Action)
update a s = a |> elim
  (\t -> (V.update t s, E.none))
  (\a -> C.update a s |> \(s', ma) ->
    ( ma |> M.map (\(dt, a') -> (V.animate dt s a' s', delay dt (Right a')))
    ) ? (s', E.none) )


app = StartApp.start
  { init = (init_state, E.none)
  , view = R.view Right C.Go C.Call
  , update = update
  , inputs = [Time.fps 30 |> S.foldp (+) 0 |> S.map Left] }

main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks
