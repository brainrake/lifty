module Lifty.Util where

import Maybe        as M
import Maybe.Extra as ME  exposing ((?))
import List        as L
import Array       as A
import Animation   as Ani
import Time               exposing (Time)
import Task               exposing (Task)
import Task.Extra
import Effects     as E   exposing (Effects, Never)
import Svg                exposing (rect)
import Svg.Attributes     exposing (x, y, width, height)

s_ f = f << toString

f_ = toFloat

zipiL l = A.toIndexedList (A.fromList l)
zipiA l = A.toIndexedList l
mapiL l f = L.map f (zipiL l)
mapiA l f = L.map f (zipiA l)

anim start from to duration =
  Ani.animation start |> Ani.from from |> Ani.to to |> Ani.duration duration

delay : Time -> a -> Effects a
delay t action = E.task <| Task.Extra.delay t <| Task.succeed <| action

schedule : (a -> b) -> Maybe (Time, a) -> Effects b
schedule act ma = M.map (\(dt, a) -> delay dt (act a)) ma ? E.none

rect_ x' y' w' h' a' =
  rect (L.append [s_ x x', s_ y y', s_ width w', s_ height h'] a') []
