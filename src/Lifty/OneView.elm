module Lifty.OneView where

import List           as L
import Array          as A exposing (Array)
import Array.Extra    as A
import Time                exposing (Time)
import Animation           exposing (Animation, animate)

import Lifty.Util    exposing (s_, f_, anim)
import Lifty.OneController as C
import Lifty.OneRender     as R


type alias Lift l = { l | y : Animation }

type alias State s l f = C.State { s | t : Time
                                     , floors: Array f } (Lift l)


update : Time -> State s l a -> State s l a
update t s = { s | t = t }

animate : Time -> State s l a -> C.Action -> State s l a -> State s l a
animate dt s a' s' = case a' of
  C.Arrive lift_id dest ->
    let l = A.getUnsafe lift_id s.lifts
        l' = A.getUnsafe lift_id s'.lifts
        y = anim s'.t (f_ l.dest) (f_ l'.dest) dt
    in { s' | lifts = A.set lift_id { l' | y = y } s'.lifts }
  _ -> s'
