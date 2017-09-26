module Render exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Models exposing (Msg, Operator(..), Expression(..))

renderExpression: Expression -> Html Msg
renderExpression e =
  div [] [text (expressionToText e)]

expressionToText: Expression -> String
expressionToText e =
  case e of
    Constant c ->
      c |> toString
    Pair e1 e2 op ->
      (expressionToText e1) ++ " " ++ (opToText op) ++ " " ++ (expressionToText e2)

opToText: Operator -> String
opToText op =
  case op of
    Add -> "+"
    Subtract -> "-"
