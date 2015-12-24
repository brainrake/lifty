module Lifty.CompareRender where

import Debug
import String               exposing (join)
import Maybe          as M
import Maybe.Extra    as M  exposing ((?))
import List           as L
import List.Extra     as L
import Array          as A
import Set
import Signal         as S
import Animation            exposing (animate)
import Html
import Svg                  exposing (svg, g, rect, circle, text', text)
import Svg.Attributes as Sa exposing (..)
import Svg.Events           exposing (..)

import Lifty.Util exposing (s_, f_, imapA, imapL, rect_)
import Lifty.OneRender    as R1
import Lifty.OneRender    as R2
import Lifty.OneSimRender as SR1
import Lifty.TwoSimRender as SR2


vPa s p y' = g []
    [ circle [ s_ cx (0.3 + animate s.t p.x)
             , s_ cy (0.69 + y')
             , r "0.3", fill "#06c", stroke "#444", strokeWidth "0.02"] []
    , text' [ s_ x (0.16 + animate s.t p.x)
            , s_ y (0.82 + y')
            , fontSize "0.4", fill "#ddd" ] [text <| toString p.dest]
    , text' [ s_ x (0.1 + animate s.t p.x)
            , s_ y (0.6 + y')
            , fontSize "0.3", fill "#ddd" ] [text <| (p.mlift |> M.map (\i -> toString i)) ? "."] ]

vFloorPax a s =
  let floors = L.groupBy (\p1 p2 -> p1.src == p2.src) s.pax
  in g [] <| imapL floors <| \(_, floor) ->
    g [] <| flip L.map (L.reverse floor) <| \(p) ->
      vPa s p <| toFloat (p.src)

vLiftPax a s =
  g [] <| imapA s.lifts <| \(lift_id, lift) ->
    g [] <| imapL lift.pax <| \(p_id, p) ->
      vPa s p <| animate s.t lift.y

vFloors a m = g []
  [ rect_ (-2) (f_ <| A.length m.floors) 0 0.04 [width "100%", fill "white"]
  , g [] <| imapA m.floors <| \(floor_id, _)  -> g []
    [ rect_ (-2) (f_ floor_id) 0 0.04 [width "100%", fill "white"]
    ]]--, text' [s_ x (-1.6), s_ y (f_ floor_id + 0.7), fontSize "0.5", fill "white"]
     --       [s_ text floor_id] ]]

vAddPax a s =
  g [] <| imapA s.s1.floors <| \(floor_id, _) ->
    let action = s.adding |> M.map (\_ -> a.endadd floor_id)
                          |> M.withDefault (a.startadd floor_id)
    in  g [ onClick <| S.message a.address action, Sa.cursor "pointer", class "addbtn"]
          [ circle [ s_ cx (8.6 + f_ (A.length s.s1.lifts + A.length s.s2.lifts))
                   , s_ cy (0.69 + toFloat floor_id), r "0.3"
                   , fill "#888", strokeWidth "0.02"] []
          , text' [ s_ x (8.47 + f_ (A.length s.s1.lifts + A.length s.s2.lifts))
                  , s_ y (0.82 + toFloat floor_id)
                  , fontSize "0.4", fill "#ddd" ] [text "+"] ]

vAddingPax a s =
  g [] <| M.withDefault [] <| flip M.map s.adding <| \(floor_id) ->
    [ circle [ s_ cx (8.6 + f_ (A.length s.s1.lifts + A.length s.s2.lifts))
             , s_ cy (0.69 + (toFloat floor_id))
             , r "0.3", fill "#08f", stroke "#444", strokeWidth "0.02"] [] ]

vCallBtns2 a m =
  g [] <| imapA m.floors <| \(floor_id, _)  ->
    g [] [ circle
            [ cx "-0.5", s_ cy (f_ floor_id + 0.72), r "0.2"
            , fill (if Set.member floor_id m.calls_up then "#0f8" else "#084")
            , opacity (if floor_id == ((A.length m.floors) - 1) then "0" else "1") ] []
         , circle
            [ cx "-0.5", s_ cy (f_ floor_id + 0.28), r "0.2"
            , fill (if Set.member floor_id m.calls_down then "#0f8" else "#084")
            , opacity (if floor_id == 0 then "0" else "1") ] [] ]

vLiftBtn1 a m lift id floor_id =
  rect_ (f_ id + 0.1) ((f_ floor_id) + 0.2) 0.8 0.84
        [ fill (if floor_id == lift.dest then "#084" else "transparent")]

vLifts1 a s =
  g [] <| imapA s.lifts <| \(lift_id, lift) -> g []
    [ rect_ (f_ lift_id + 0.1) 0.04  0.8 ((f_ <| A.length s.floors) - 0.04)
            [ fill "#000", opacity "0.7"]
    , g [] <| imapA s.floors <| \(floor_id, _) ->
        vLiftBtn1 a s lift lift_id floor_id
    , R1.vLift a s lift_id lift ]

vLiftBtn2 a m lift id floor_id =
  rect_ (f_ id + 0.1) ((f_ floor_id) + 0.2) 0.8 0.84
        [ fill (if Set.member floor_id lift.dests then "#084" else "transparent")]

vLifts2 a s =
  g [] <| imapA s.lifts <| \(lift_id, lift) -> g []
    [ rect_ (f_ lift_id + 0.1) 0.04  0.8 ((f_ <| A.length s.floors) - 0.04)
            [ fill "#000", opacity "0.7"]
    , g [] <| imapA s.floors <| \(floor_id, _) ->
        vLiftBtn2 a s lift lift_id floor_id
    , rect_ (0.45 + f_ lift_id) (0.55 + f_ lift.next) 0.1 0.1 [fill "#444"]
    , R1.vLift a s lift_id lift ]

view act startadd endadd address s =
  let a = { act = act, startadd = startadd, endadd = endadd, address = address }
      w = 4 + 7 + toFloat ((A.length s.s1.lifts) + (A.length s.s2.lifts))
      h = 1 + toFloat (A.length s.s1.floors)
  in Html.div [] [R1.style_, svg
    [ x "0", y "0", s_ width ( 20 * w) , s_ height (20 * h)
    , R1.vbox (-2) (-0.5) w h ]
    [ g []
        [ g []
            [ R1.vBg
            , R1.vFloors a s.s1
            --, R1.vCallBtns a s1
            , vLifts1 a s.s1
            , SR1.vFloorPax a s.s1
            , SR1.vLiftPax a s.s1
            , SR1.vLeavingPax a s.s1
            ]
        , g [transform ("translate ("++ toString (5 + A.length s.s1.lifts) ++ ", 0)")]
            [ R1.vBg
            , vFloors a s.s2
            , vLifts2 a s.s2
            , vCallBtns2 a s.s2
            , SR2.vFloorPax a s.s2
            , SR2.vLiftPax a s.s2
            , SR2.vLeavingPax a s.s2
            ]
        , g []
            [ vAddPax a s
            , vAddingPax a s
            ]

        ]
    ] ]
