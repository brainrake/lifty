module Lifty.TwoView where

import List           as L
import Array          as A   exposing (Array)
import Array.Extra    as A
import Time                  exposing (Time)
import Animation      as Ani exposing (Animation, animate, retarget, ease, duration, static)
import Easing                exposing (easeInSine, easeOutSine, linear)

import Lifty.Util exposing (s_, f_, anim)
import Lifty.TwoController   as C
import Lifty.TwoRender       as R

type alias Lift l = { l | y : Animation }

type alias State s l a = C.State { s | t : Time} (Lift l) a

update t s = { s | t = t }

animate : Time -> C.Action -> State s l a -> C.Action -> State s l a -> State s l a
animate dt a s a' s' =
  let s'' = case a of
    C.Approach lift_id dest ->
      let l' = A.getUnsafe lift_id s'.lifts
          y = anim (s'.t) (Ani.animate s'.t l'.y) (f_ dest) dt |> ease identity
      in { s' | lifts = A.set lift_id { l' | y = y } s'.lifts }
    _ -> s'
  in case a' of
    C.Arrive lift_id dest ->
      let l' = A.getUnsafe lift_id s''.lifts
          y = anim (s''.t) (Ani.animate s'.t l'.y) (f_ dest) dt |> ease identity
      in { s'' | lifts = A.set lift_id { l' | y = y } s''.lifts }
    _ -> s''
