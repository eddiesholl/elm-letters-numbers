module Attempts exposing (..)

import Models exposing (Inputs,  Expression(..), Operator(..))

generateAttempts: Inputs -> List Expression
generateAttempts inputs =
  generateAttemptsArgs inputs [Add, Subtract]

generateAttemptsArgs: Inputs -> List Operator -> List Expression
generateAttemptsArgs inputs args =
  case inputs of
    i1::i2::iRest ->
      let
        c1 = Constant i1
        c2 = Constant i2
      in
        [c1] ++ (eachArg args c1 c2) ++ (generateAttemptsArgs (i2::iRest) args)
    [i1] ->
      [Constant i1]
    _ ->
      []


eachArg: List Operator -> Expression -> Expression -> List Expression
eachArg args e1 e2 =
  List.map (Pair e1 e2) args
