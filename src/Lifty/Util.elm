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

-- Return a list containing the integers from zero to n-1
zeroTo : Int -> List Int
zeroTo n = L.repeat n () |> L.indexedMap (\a b -> a)

-- Same as `zipWithIndex`
izipL : List a -> List (Int, a)
izipL l = A.toIndexedList (A.fromList l)

-- Same as `zipWithIndex`, start with an Array
izipA : Array a -> List (Int, a)
izipA l = A.toIndexedList l

-- Same as `flip mapWithIndex`
imapL : List a -> ((Int, a) -> b) -> List b
imapL l f = L.indexedMap (curry f) l

-- Same as `flip mapWithIndex`, start with an array
imapA : Array a -> ((Int, a) -> b) -> List b
imapA l f = L.map f (izipA l)

-- Split the list into the first `n` items where the predicate holds, and the rest
partitionUpto : Int -> (a -> Bool) -> List a -> (List a, List a)
partitionUpto n pred xs = let
  aux n xs (trues, falses) = case xs of
    [] -> (trues, falses)
    x :: xs' -> if n > 0 then if pred x then aux (n-1) xs' (x :: trues, falses)
                                        else aux (n-1) xs' (trues, x :: falses)
                         else aux n xs' (trues, x :: falses)
  in aux n xs ([], [])

-- Shorthand for defining an animation with common parameters
anim start from to duration =
  Ani.animation start |> Ani.from from |> Ani.to to |> Ani.duration duration

-- Return the effect resulting from delaying an action
delay : Time -> a -> Effects a
delay t action = E.task <| Task.Extra.delay t <| Task.succeed <| action

-- Return an effect resulting from transforming and delaying an action, or none
schedule : (a -> b) -> Maybe (Time, a) -> Effects b
schedule act ma = M.map (\(dt, a) -> delay dt (act a)) ma ? E.none

-- Given an `Address a` and a function that returns `a`,
-- return a function that returns a `Message`
mkM : Address a -> (b -> a) -> (b -> Message)
mkM address action param = S.message address (action param)

-- Given an `Address a` and a two-parameter function that returns `a`,
-- return a two-parameter function that returns a `Message`
mkM2 : Address a -> (c -> b -> a) -> (c -> b -> Message)
mkM2 address action param1 param2 = S.message address (action param1 param2)
