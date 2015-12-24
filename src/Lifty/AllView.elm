module Lifty.AllView where

import Maybe          as M
import Maybe.Extra    as M exposing ((?))
import List           as L
import List.Extra     as L
import Array          as A exposing (Array)
import Array.Extra    as A
import Time                exposing (Time)
import Either              exposing (Either(..))
import Animation           exposing (Animation, animate, static, retarget, ease)
import Effects        as E

import Lifty.Util               exposing (s_, f_, imapL, izipL, anim)
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

--update : Time -> State s l -> State s l
update a s = case a of
  StartAdd src -> ({ s | adding = Just src }, Nothing)
  FinishAdd dest ->
    s.adding |> M.map (\src ->
      let p = { src = src
              , dest = dest
              , x = static (2.3 + f_ (A.length s.lifts))
              , mlift = Nothing }
      in ({ s | adding = Nothing }, Just (C.AddPassenger p)))
    |> M.withDefault (s, Nothing)
  Tick t -> ({ s | t = t }, Nothing)


--animate : Time -> C.Action p -> State s l p -> C.Action p -> State s' l p -> State s' l p
animate a s ma s' =
  let s'' = case a of
    C.AddPassenger p -> let
      len = s.pax |> L.filter (\p' -> p'.src == p.src) |> L.length
      p' = L.head s'.pax ? p
      p'' = { p' | x = anim s'.t (2.3 + f_ (A.length s.lifts)) (2 + (f_ len) / 3) 500 }
      in { s' | pax = p'' :: L.tail s'.pax ? s.pax }
    C.Approach lift_id floor_id ->
      let l = A.getUnsafe lift_id s.lifts
          l' = A.getUnsafe lift_id s'.lifts
          y = anim s'.t (f_ l.next) (f_ floor_id) 1000 |> ease identity
      in { s' | lifts = A.set lift_id { l' | y = y } s'.lifts }
    C.Arrive lift_id floor_id ->
      animate_arrived lift_id floor_id s s'
    _ -> s'
  in case ma of
    Just (dt, (C.Arrive lift_id floor_id)) ->
      let l = A.getUnsafe lift_id s.lifts
          l' = A.getUnsafe lift_id s''.lifts
          y = anim s''.t (f_ l.next) (f_ l'.next) dt |> ease identity
      in { s'' | lifts = A.set lift_id { l' | y = y } s''.lifts }
    _ -> s''

animate_arrived lift_id floor_id s s' = let
  l = A.getUnsafe lift_id s'.lifts
  lpax' = imapL l.pax (\(i, p) ->
    { p | x = retarget s'.t (f_ lift_id + (f_ i) / 3) p.x })
  lifts' = A.set lift_id { l | pax = lpax' } s'.lifts
  f = izipL s'.pax |> L.filter (\(ip, p) -> p.src == floor_id)
  f' =  L.reverse <| imapL (L.reverse f) (\(i, (pi, p)) ->
    (pi, { p | x = retarget s'.t ((f_ (A.length s'.lifts)) + (f_ i) / 3) p.x}))
  pax' = A.toList (L.foldr (\(pi, p) apax -> A.set pi p apax) (A.fromList s'.pax) f')
  ldiff = L.length s'.leaving - L.length s.leaving
  (new_leaving, old_leaving) = L.splitAt ldiff s'.leaving
  new_leaving' = new_leaving |> L.map (\p ->
    { p | x = retarget s.t (-3) p.x })
  leaving' = L.append new_leaving' old_leaving
  in { s' | lifts = lifts', leaving = leaving', pax = pax'}
