module Comparers exposing (..)

import Models exposing (Expression(..), Operator(..))

operatorPrecedence op =
  case op of
    Add -> 1
    Subtract -> 2

compareExpressions a b =
  case a of
    Constant c1 ->
      case b of
        Constant c2 ->
          compare c1 c2
        Pair _ _ _ ->
          LT
    Pair e1a e2a opa ->
      case b of
        Constant _ ->
          GT
        Pair e1b e2b opb ->
          let
            sortE1 = compareExpressions e1a e1b
            sortE2 = compareExpressions e2a e2b
            sortOp = compareOperators opa opb
          in
            case sortE1 of
              EQ ->
                case sortE2 of
                  EQ ->
                    sortOp
                  _ -> sortE2
              _ -> sortE1

compareOperators a b =
  let
    precA = operatorPrecedence a
    precB = operatorPrecedence b
  in
    compare precA precB
