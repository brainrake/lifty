module Lifty.AllView where

import Maybe          as M
import Maybe.Extra    as M   exposing ((?))
import List           as L
import List.Extra     as L
import Array          as A   exposing (Array)
import Array.Extra    as A
import Time                  exposing (Time)
import Either                exposing (Either(..))
import Animation      as Ani exposing (Animation, animate, static, retarget, ease)
import Effects        as E

import Lifty.Util               exposing (s_, f_, imapL, izipL, anim, delay)
import Lifty.OneSimView         exposing (animate_arrived)
import Lifty.OneController      exposing (LiftId, FloorId)
import Lifty.AllController as C
import Lifty.AllRender     as R


type Action = StartAdd FloorId
            | FinishAdd FloorId
            | Tick Time

type alias Passenger p = { p | x : Animation }

type alias Lift l = { l | y : Animation
                        , next : FloorId }

type alias State s l p = { s | t : Time
                             , lifts : Array (Lift l)
                             , adding : Maybe FloorId
                             , leaving : List (Passenger p) }


-- update : C.Action p -> State s l p -> (State s l p, Maybe (C.Action p))
update a s = case a of
  StartAdd src -> ({ s | adding = Just src }, E.none)
  FinishAdd dest ->
    s.adding |> M.map (\src ->
      let p =
        { src = src
        , dest = dest
        , x = let
            floor_len = L.length (s.pax |> L.filter (\p -> p.src == src))
            num_lifts = f_ (A.length s.lifts)
            in anim s.t (2.3 + num_lifts) (num_lifts + (f_ floor_len) / 4) 500
        , mlift = Nothing }
      in ({ s | adding = Nothing }, delay 0 (C.AddPassenger p)))
    |> M.withDefault (s, E.none)
  Tick t -> ({ s | t = t }, E.none)

animate s a s' ma =
  let s'' = case a of
    C.Approach lift_id dest ->
      let l' = A.getUnsafe lift_id s'.lifts
          y = anim (s'.t) (Ani.animate s'.t l'.y) (f_ dest) 1000 |> ease identity
      in { s' | lifts = A.set lift_id { l' | y = y } s'.lifts }
    (C.Arrive lift_id floor_id) ->
      animate_arrived lift_id floor_id s s'
    _ -> s'
  in case ma of
    Just (dt, C.Arrive lift_id dest) ->
      let l' = A.getUnsafe lift_id s''.lifts
          y = anim (s''.t) (Ani.animate s'.t l'.y) (f_ dest) dt |> ease identity
      in { s'' | lifts = A.set lift_id { l' | y = y } s''.lifts }
    _ -> s''

animate_arrived lift_id floor_id s s' = let
  l = A.getUnsafe lift_id s'.lifts
  lpax' = imapL l.pax (\(i, p) ->
    let to = f_ lift_id + 0.05 + 0.76 * f_ i / f_ s.lift_cap
                               + 0.76 / (2 * f_ s.lift_cap)
    in { p | x = anim (s.t) (Ani.animate (s.t) (p.x)) to 1000 })
  lifts' = A.set lift_id { l | pax = lpax' } s'.lifts
  f = izipL s'.pax |> L.filter (\(ip, p) -> p.src == floor_id)
  f' =  L.reverse <| imapL (L.reverse f) (\(i, (pi, p)) ->
    (pi, { p | x = retarget s'.t ((f_ (A.length s'.lifts)) + (f_ i) / 3) p.x}))
  pax' = A.toList (L.foldr (\(pi, p) apax -> A.set pi p apax) (A.fromList s'.pax) f')
  ldiff = L.length s'.leaving - L.length s.leaving
  (new_leaving, old_leaving) = L.splitAt ldiff s'.leaving
  new_leaving' = new_leaving |> L.map (\p ->
    { p | x = anim s'.t (Ani.animate s'.t p.x) (-0.3) 800 })
  leaving' = L.append new_leaving' old_leaving
  in { s' | lifts = lifts', leaving = leaving', pax = pax' }
