module Attempts exposing (..)

import Models exposing (Inputs,  Expression(..), Operator(..), PairedOperator, Pair)

opReverse op =
  case op of
    Add -> False
    Subtract -> True

generateAttempts: Inputs -> List Expression
generateAttempts inputs =
  inputs |> inputsToConstants |> (deriveExpressions [Add, Subtract])

inputsToConstants =
  List.map ConstExp

deriveExpressions: List Operator -> List Expression -> List Expression
deriveExpressions args inputs =
  case inputs of
    [e1, e2] ->
      [e1, e2] ++ (applyArgs e1 e2 args)
    [e1] ->
      [e1]
    [] ->
      []
    e1::eRest ->
      crossApplyExpr e1 (deriveExpressions args eRest)

applyArgs e1 e2 args =
  List.concatMap (applyArg e1 e2) args

applyArg e1 e2 arg =
  case (opReverse arg) of
    True -> [(buildOpExp e1 e2 arg), (buildOpExp e2 e1 arg)]
    False -> [buildOpExp e1 e2 arg]

buildOpExp left right arg =
  OpExp (PairedOperator (Pair left right) arg)

-- eachArg: List Operator -> Expression -> Expression -> List Expression
-- eachArg args e1 e2 =
--   List.map (OpExp e1 e2) args

crossApplyExpr subject targets =
  []