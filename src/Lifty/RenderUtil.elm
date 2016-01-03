module Lifty.RenderUtil where

import String               exposing (join)
import Svg                  exposing (Svg, Attribute, svg, rect, g, circle, text', text)
import Svg.Attributes as SA exposing (x, y, width, height, cx, cy, r, transform, viewBox)

import Lifty.Util exposing (f_, s_)


-- Translate a list of elements
movexy : number1 -> number2 -> List Svg -> Svg
movexy x y els=
  g [ transform ("translate(" ++ (s_ x) ++ "," ++ (s_ y) ++ ")") ] els

-- Translate a list of elements horizontally
movex : number -> List Svg -> Svg
movex x els = movexy x 0 els

-- Translate a list of elements vertically
movey : number -> List Svg -> Svg
movey y els = movexy 0 y els

-- Shorthand for rectangle with common attributes
rect_ : number1 -> number2 -> number3 -> number4 -> List Attribute -> Svg
rect_ x' y' w' h' attrs =
  rect ([x (s_ x'), y (s_ y'), width (s_ w'), height (s_ h')] ++ attrs) []

-- Shorthand for circle with common attributes
circle_ : number1 -> number2 -> number3 -> List Attribute -> Svg
circle_ cx' cy' r' attrs =
  circle ([cx (s_ cx'), cy (s_ cy'), r (s_ r')] ++ attrs) []

-- Shorthand for text with common attributes
text_ : String -> number1 -> number2 -> List Attribute -> Svg
text_ str x' y' attrs = text' ([x (s_ x'), y (s_ y')] ++ attrs) [text str]

-- Shorthand for viewBox with common attributes
vbox : number1 -> number2 -> number3 -> number4 -> Attribute
vbox x y w h = viewBox <| join " " <| [s_ x, s_ y, s_ w, s_ h]
