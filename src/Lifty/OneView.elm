module Lifty.OneView where

import List           as L
import Array          as A exposing (Array)
import Array.Extra    as A
import Time                exposing (Time)
import Animation           exposing (Animation, animate)

import Lifty.Util    exposing (s_, f_, anim)
import Lifty.OneController as C exposing (FloorId)
import Lifty.OneRender     as R


type alias Lift l = { l | y : Animation
                        , dest : FloorId }

type alias State s l = { s | t : Time
                           , lifts : Array (Lift l) }


update : Time -> State s l -> State s l
update t s = { s | t = t }

animate : Time -> State s l -> C.Action -> State s l -> State s l
animate dt s a' s' = case a' of
  C.Arrive lift_id floor_id ->
    let l = A.getUnsafe lift_id s.lifts
        l' = A.getUnsafe lift_id s'.lifts
        y = anim s'.t (f_ l.dest) (f_ l'.dest) dt
    in { s' | lifts = A.set lift_id { l' | y = y } s'.lifts }
  _ -> s'
