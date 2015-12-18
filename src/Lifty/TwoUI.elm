module Lifty.TwoUI where

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
import StartApp

import Lifty.Util    exposing (delay)
import Lifty.TwoController as C
import Lifty.TwoView       as V
import Lifty.TwoRender     as R


type alias Action = Either Time C.Action

init_state = { t = 0
             , calls_up = Set.fromList []
             , calls_down = Set.fromList []
             , floors = A.repeat 5 ()
             , lifts = A.repeat 2 { dests = Set.fromList []
                                  , last = 0
                                  , busy = False
                                  , up = False
                                  , y = static 0 } }


--update : Action -> V.State s l a -> (V.State s l a, Effects Action)
update a s = a |> elim
  (\t -> (V.update t s, E.none))
  (\a -> C.update (Debug.log "a" a) s |> \(s', ma) ->
    ( ma |> M.map (\(dt, a') -> (V.animate dt a s a' s', delay dt (Right a')))
    ) ? (s', E.none) )


app = StartApp.start
  { init = (init_state, E.none)
  , view = R.view Right C.Go C.CallUp C.CallDown
  , update = update
  , inputs = [Time.fps 30 |> S.foldp (+) 0 |> S.map Left] }

main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks
