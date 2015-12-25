module Lifty.Util where

import Maybe       as M
import Maybe.Extra as ME  exposing ((?))
import List        as L
import Array       as A   exposing (Array)
import Signal      as S   exposing (Address, Message)
import Animation   as Ani
import Time               exposing (Time)
import Task               exposing (Task)
import Task.Extra
import Effects     as E   exposing (Effects, Never)
import Svg                exposing (rect)
import Svg.Attributes     exposing (x, y, width, height)


s_ = toString

f_ = toFloat

zeroTo : Int -> List Int
zeroTo n = L.repeat n () |> L.indexedMap (\a b -> a)

izipL : List a -> List (Int, a)
izipL l = A.toIndexedList (A.fromList l)

izipA : Array a -> List (Int, a)
izipA l = A.toIndexedList l

imapL : List a -> ((Int, a) -> b) -> List b
imapL l f = L.indexedMap (curry f) l

imapA : Array a -> ((Int, a) -> b) -> List b
imapA l f = L.map f (izipA l)

anim start from to duration =
  Ani.animation start |> Ani.from from |> Ani.to to |> Ani.duration duration

delay : Time -> a -> Effects a
delay t action = E.task <| Task.Extra.delay t <| Task.succeed <| action

schedule : (a -> b) -> Maybe (Time, a) -> Effects b
schedule act ma = M.map (\(dt, a) -> delay dt (act a)) ma ? E.none

mkM : Address a -> (b -> a) -> b -> Message
mkM address action param = S.message address (action param)

mkM2 : Address a -> (c -> b -> a) -> c -> b -> Message
mkM2 address action param1 param2 = S.message address (action param1 param2)
