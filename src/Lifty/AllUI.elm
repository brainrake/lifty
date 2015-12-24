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
import StartApp

import Lifty.Util    exposing (delay, schedule)
import Lifty.AllController as C
import Lifty.AllView       as V
import Lifty.AllRender     as R


type alias Action = Either V.Action (C.Action (V.Passenger {}))

type alias State = V.State (C.State {} {} () {}) {} {}

init_state = { t = 0
             , floors = A.repeat 5 ()
             , pax = []
             , adding = Nothing
             , leaving = []
             , lifts = A.repeat 1 { busy = False
                                  , up = False
                                  , next = 0
                                  , pax = []
                                  , cap = 2
                                  , y = static 0 } }

schedule_ = schedule Right

update_c a s =
  C.update (Debug.log "a" a) s |> \(s', ma) -> (V.animate a s ma ( s'), schedule_ ma)

update a s = a |> elim
  (\va -> V.update va s |> \(s', ma) ->
    (ma |> M.map (\ca -> update_c ca s')) ? (s', E.none))
  (\ca -> update_c ca s)


app = StartApp.start
  { init = (init_state, E.none)
  , view = R.view (Left << V.StartAdd)
                  (Left << V.FinishAdd)
  , update = update
  , inputs = [Time.fps 30 |> S.foldp (+) 0 |> S.map (Left << V.Tick)] }

main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks












