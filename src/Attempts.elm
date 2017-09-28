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
      [e1, e2] ++ (applyArgs e1 args e2)
    [e1] ->
      [e1]
    [] ->
      []
    e1::eRest ->
      crossApplyExp e1 args (deriveExpressions args eRest)

applyArgs left args right =
  List.concatMap (applyArg left right) args

applyArg left right arg =
  case (opReverse arg) of
    True -> [(buildOpExp left right arg), (buildOpExp right left arg)]
    False -> [buildOpExp left right arg]

buildOpExp left right arg =
  OpExp (PairedOperator (Pair left right) arg)

-- eachArg: List Operator -> Expression -> Expression -> List Expression
-- eachArg args e1 e2 =
--   List.map (OpExp e1 e2) args

crossApplyExp subject args targets =
  let
    applied = List.concatMap (applyArgs subject args) targets
  in
    [subject] ++ targets ++ applied
