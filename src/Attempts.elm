module Attempts exposing (..)

import Models exposing (Inputs,  Expression(..), Operator(..))

generateAttempts: Inputs -> List Expression
generateAttempts inputs =
  (generateAttemptsConsts inputs) ++
  (generateAttemptsArgs inputs [Add, Subtract])

generateAttemptsConsts inputs =
  List.map Constant inputs

generateAttemptsArgs: Inputs -> List Operator -> List Expression
generateAttemptsArgs inputs args =
  case inputs of
    i1::i2::iRest ->
      let
        c1 = Constant i1
        c2 = Constant i2
      in
        (eachArg args c1 c2) ++ (generateAttemptsArgs (i2::iRest) args)
    _ ->
      []


eachArg: List Operator -> Expression -> Expression -> List Expression
eachArg args e1 e2 =
  List.map (Pair e1 e2) args
