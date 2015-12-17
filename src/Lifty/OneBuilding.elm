module Lifty.OneBuilding where

import Debug
import Maybe        as M
import List         as L
import List.Extra   as LE
import Array        as A  exposing (Array)
import Array.Extra  as AE
import Signal       as S
import Signal.Extra as SE
import Task
import Task.Extra
import Time
import Effects      as E
import Animation    as Ani exposing (Animation, animation, static,  animate)
import StartApp

import Lifty.OneController   as C
import Lifty.OneBuildingView as V


type alias FloorId = Int

type alias Passenger =
  { dest: FloorId
  , ani: Animation}

type alias Model =
  { t : Time.Time
  , lifts : C.Model { pax : List Passenger, ani : Animation }
  , floors : Array (List Passenger)
  , leaving : List Passenger}

type Action = AddPassenger FloorId FloorId
            | Action C.Action
            | Tick Time.Time

delay t act = E.task <| Task.Extra.delay t <| Task.succeed <| Action act

init_model : Model
init_model =
  { t = 0
  , floors = A.repeat 5 []
  , lifts = A.repeat 2 { dest = 0, busy = False, pax = [], ani = static 0}
  , leaving = [] }

max_queue = 4
lift_cap = 2

update : Action -> Model -> (Model, E.Effects Action)
update action model = case action of
  AddPassenger src dest ->
    let floor = AE.getUnsafe src model.floors
        floor' = ({ dest = dest, ani = static (toFloat src) } :: floor)
    in if L.length floor < max_queue
       then ({model | floors = A.set src floor' model.floors}, E.task <| Task.succeed <| Action <| C.Call src)
       else (model, E.none)
  Action a ->
    case C.update (Debug.log "action" a) model.lifts of (lifts', mayact) ->
      let (model', eff) = case mayact of
        Just (dt, act) -> case act of
            C.Arrive i to ->
              let l = AE.getUnsafe i model.lifts
                  l' = AE.getUnsafe i lifts'
                  ani = animation model.t |> Ani.from (toFloat l.dest) |> Ani.to (toFloat l'.dest) |> Ani.duration dt
                  lifts'' = A.set i { l' | ani = ani} lifts'
              in ({ model | lifts = lifts'' }, delay dt act)
            _ -> ({ model | lifts = lifts' }, delay dt act)
        Nothing -> ({ model | lifts = lifts' }, E.none)
      in case a of
        C.Arrive lift_id floor_id ->
          let floor = AE.getUnsafe floor_id model'.floors
              lift = AE.getUnsafe lift_id model'.lifts
              (leaving, pax') = lift.pax |> L.partition (\p -> p.dest == floor_id)
              spaces = lift_cap - L.length pax'
              (entering, floor') = (L.take spaces floor, L.drop spaces floor)
              pax'' = L.append pax' entering
              lift' = { lift | pax = pax'' }
              model'' = { model' | floors = A.set floor_id floor' model'.floors
                                 , lifts = A.set lift_id lift' model'.lifts
                                 , leaving = L.append leaving model'.leaving }
              in (model'', eff)
        C.Idle lift_id ->
          let lift = AE.getUnsafe lift_id model'.lifts
          in if L.isEmpty lift.pax
             then model'.floors
               |> A.indexedMap (,)
               |> A.filter (\(_, floor) -> not (L.isEmpty floor))
               |> A.get 0
               |> M.map (\(floor_id, _) ->
                 case update (Action (C.Call floor_id)) model' of
                   (model'', eff') -> (model'', E.batch [eff, eff']))
               |> M.withDefault (model', eff)
             else
               let dest = lift.pax |> L.map (\p -> p.dest)
                                   |> LE.minimumBy (\d -> abs (lift.dest - d))
                                   |> M.withDefault lift.dest
               in update (Action (C.Go lift_id dest)) model'
        _ -> (model', eff)



  Tick t -> ({ model | t = t }, E.none)

app = StartApp.start
  { init = (init_model, E.none)
  , view = V.view Action AddPassenger
  , update = update
  , inputs = [Time.fps 30 |> S.foldp (+) 0 |> S.map Tick] }

main = app.html

port tasks : Signal (Task.Task E.Never ())
port tasks = app.tasks

