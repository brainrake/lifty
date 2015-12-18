module Lifty.OneSimView where

import Maybe          as M
import List           as L
import Array          as A   exposing (Array)
import Array.Extra    as AE
import Time                  exposing (Time)
import Animation      as Ani exposing (Animation, animate, retarget)
import Effects        as E   exposing (Effects, Never)

import Lifty.Util    exposing (s_, f_, zipiL, mapiA, anim, delay)
import Lifty.OneController as C
import Lifty.OneView       as V
import Lifty.OneSim        as Sim

type alias Passenger p = { p | x: Animation }

type Action = StartAdd C.FloorId
            | FinishAdd C.FloorId
            | Tick Time

type alias State s l p = V.State { s | adding : Maybe C.FloorId
                                     , leaving : List (Passenger p) }
                                 l
                                 (List (Passenger p))


--update : Action p -> State s l p -> (State s l p, Effects Action)
update a s = case a of
  StartAdd src -> ({ s | adding = Just src }, E.none)
  FinishAdd dest ->
    s.adding
    |> M.map (\src ->
      let floor = (AE.getUnsafe src s.floors)
          x = anim s.t 4.3  (2 + (f_ <| L.length floor) / 3) 500
      in Sim.update (Sim.AddPassenger src dest { x = x, dest = dest })
                    { s | adding = Nothing }
    |> \(s, ma, e) -> (s, e))
    |> M.withDefault (s, E.none)
  Tick t -> (V.update t s, E.none)


--animate : State s l p -> State s l p -> Time -> C.Action -> State s l p
animate s s' dt a' = case a' of
  C.Arrive lift_id dest ->
    let l = AE.getUnsafe lift_id s.lifts
        l' = AE.getUnsafe lift_id s'.lifts
        y = anim s'.t (f_ l.dest) (f_ l'.dest) dt
        (ileaving, ipax') = l.pax |> zipiL
                          |> L.partition (\(_, p) -> p.dest == dest)
        floor = AE.getUnsafe dest s'.floors
        floor' = floor |> L.map (\p -> {p | x = p.x})
    in { s' | floors = A.set dest floor' s'.floors
            , lifts = A.set lift_id { l' | y = y } s'.lifts
            }
  _ -> s'



      --C.Arrive lift_id floor_id ->
      --  let floor = AE.getUnsafe floor_id state'.floors
      --      lift = AE.getUnsafe lift_id state'.lifts
      --      (ileaving, ipax') = lift.pax
      --                        |> A.fromList |> A.toIndexedList
      --                        |> L.partition (\(p_id, p) -> p.dest == floor_id)
      --      --pax' = L.map snd ipax'
      --      leaving' = ileaving |> L.map (\(p_id, p) -> { p | ani =
      --        retarget state.t (-3) p.ani })
      --      spaces = lift_cap - L.length ipax'
      --      irfloor = L.reverse <| zipiL floor
      --      (ientering, ifloor') = ( L.take spaces irfloor
      --                             , L.drop spaces <| irfloor)
      --      ipax'' = L.append ipax' ientering
      --      pax'' =  mapiL ipax'' <| \(new, (old, p)) ->
      --        { p | ani = retarget state.t (f_ lift_id + (f_ new) / 3) p.ani}
      --      lift' = { lift | pax = pax'' }
      --      floor' = mapiL (ifloor') <| \(new, (old, p)) ->
      --        { p | ani = retarget state.t (2 + (f_ new) / 3) p.ani}
      --      state'' = { state' | floors = A.set floor_id floor' state'.floors
      --                         , lifts = A.set lift_id lift' state'.lifts
      --                         , leaving = L.append leaving' state'.leaving }
      --      in (state'', eff)
      --C.Idle lift_id ->
      --  let lift = AE.getUnsafe lift_id state'.lifts
      --  in if L.isEmpty lift.pax
      --     then state'.floors
      --       |> zipiA
      --       |> L.filter (\(_, floor) -> not (L.isEmpty floor))
      --       |> L.head
      --       |> M.map (\(floor_id, _) ->
      --         case update (Action (C.Call floor_id)) state' of
      --           (state'', eff') -> (state'', E.batch [eff, eff']))
      --       |> M.withDefault (state', eff)
      --     else
      --       let dest = lift.pax |> L.map (\p -> p.dest)
      --                           |> LE.minimumBy (\d -> abs (lift.dest - d))
      --                           |> M.withDefault lift.dest
      --       in update (Action (C.Go lift_id dest)) state'
      --_ -> (s', eff)











--module Lifty.OneBuilding where

--import Debug
--import Maybe        as M
--import List         as L
--import List.Extra   as LE
--import Array        as A  exposing (Array)
--import Array.Extra  as AE
--import Signal       as S
--import Signal.Extra as SE
--import Task
--import Task.Extra
--import Time
--import Effects      as E
--import Animation    as Ani exposing (Animation, static, retarget)

--import Lifty.Util exposing (f_, zipiL, zipiA, mapiL, mapiA, anim)
--import Lifty.OneController   as C
--import Lifty.OneBuildingView as V


--type alias FloorId = Int

--type alias Passenger = { dest: FloorId
--                       , x: Animation }

--type alias Lift l = { l | pax : List Passenger
--                        , y : Animation }

--type alias State s l = C.State { s | t : Time.Time
--                                   , floors : Array (List Passenger)
--                                   , adding : Maybe FloorId
--                                   , leaving : List Passenger } (Lift l)

--type Action = StartAddPassenger FloorId
--            | EndAddPassenger FloorId
--            | AddPassenger FloorId FloorId
--            | Action C.Action
--            | Tick Time.Time

--delay t act = E.task <| Task.Extra.delay t <| Task.succeed <| Action act

--init_state : State
--init_state =
--  { t = 0
--  , floors = A.repeat 5 []
--  , cstate = A.repeat 2 { dest = 0, busy = False, pax = [], ani = static 0}
--  , adding = Nothing
--  , leaving = [] }

--max_queue = 4
--lift_cap = 2

--update : Action -> State -> (State, E.Effects Action)
--update action state = case action of
--  StartAddPassenger src ->
--    ({ state | adding = Just src }, E.none)
--  EndAddPassenger dest ->
--    state.adding |> M.map (\src -> update (AddPassenger src dest)
--                                          { state | adding = Nothing })
--                 |> M.withDefault (state, E.none)
--  AddPassenger src dest ->
--    let floor = AE.getUnsafe src state.floors
--        ani = anim state.t 4.3  (2 + (f_ <| L.length floor) / 3) 500
--        floor' = ({ dest = dest, ani = ani } :: floor)
--    in if L.length floor < max_queue
--       then ( { state | floors = A.set src floor' state.floors }
--            , E.task <| Task.succeed <| Action <| C.Call src )
--       else (state, E.none)
--  Action a ->
--    case C.update (Debug.log "action" a) state.lifts of (lifts', mayact) ->
--      let (state', eff) = case mayact of
--        Just (dt, act) -> case act of
--            C.Arrive i to ->
--              let l = AE.getUnsafe i state.lifts
--                  l' = AE.getUnsafe i lifts'
--                  ani = anim state.t (f_ l.dest) (f_ l'.dest) dt
--                  lifts'' = A.set i { l' | ani = ani} lifts'
--              in ({ state | lifts = lifts'' }, delay dt act)
--            _ -> ({ state | lifts = lifts' }, delay dt act)
--        Nothing -> ({ state | lifts = lifts' }, E.none)
--      in case a of
--        C.Arrive lift_id floor_id ->
--          let floor = AE.getUnsafe floor_id state'.floors
--              lift = AE.getUnsafe lift_id state'.lifts
--              (ileaving, ipax') = lift.pax
--                                |> A.fromList |> A.toIndexedList
--                                |> L.partition (\(p_id, p) -> p.dest == floor_id)
--              --pax' = L.map snd ipax'
--              leaving' = ileaving |> L.map (\(p_id, p) -> { p | ani =
--                retarget state.t (-3) p.ani })
--              spaces = lift_cap - L.length ipax'
--              irfloor = L.reverse <| zipiL floor
--              (ientering, ifloor') = ( L.take spaces irfloor
--                                     , L.drop spaces <| irfloor)
--              ipax'' = L.append ipax' ientering
--              pax'' =  mapiL ipax'' <| \(new, (old, p)) ->
--                { p | ani = retarget state.t (f_ lift_id + (f_ new) / 3) p.ani}
--              lift' = { lift | pax = pax'' }
--              floor' = mapiL (ifloor') <| \(new, (old, p)) ->
--                { p | ani = retarget state.t (2 + (f_ new) / 3) p.ani}
--              state'' = { state' | floors = A.set floor_id floor' state'.floors
--                                 , lifts = A.set lift_id lift' state'.lifts
--                                 , leaving = L.append leaving' state'.leaving }
--              in (state'', eff)
--        C.Idle lift_id ->
--          let lift = AE.getUnsafe lift_id state'.lifts
--          in if L.isEmpty lift.pax
--             then state'.floors
--               |> zipiA
--               |> L.filter (\(_, floor) -> not (L.isEmpty floor))
--               |> L.head
--               |> M.map (\(floor_id, _) ->
--                 case update (Action (C.Call floor_id)) state' of
--                   (state'', eff') -> (state'', E.batch [eff, eff']))
--               |> M.withDefault (state', eff)
--             else
--               let dest = lift.pax |> L.map (\p -> p.dest)
--                                   |> LE.minimumBy (\d -> abs (lift.dest - d))
--                                   |> M.withDefault lift.dest
--               in update (Action (C.Go lift_id dest)) state'
--        _ -> (state', eff)

--  Tick t ->
--    ( { state | t = t
--              , leaving = state.leaving |> L.filter (\(p) -> Ani.isRunning t p.ani) }
--    , E.none)
