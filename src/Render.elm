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
    ConstExp c ->
      c |> toString
    OpExp opExp ->
      "( " ++ (expressionToText opExp.pair.left) ++ " " ++ (opToText opExp.op) ++ " " ++ (expressionToText opExp.pair.right) ++ " )"

opToText: Operator -> String
opToText op =
  case op of
    Add -> "+"
    Subtract -> "-"
    Multiply -> "x"
    -- Divide -> "/"
