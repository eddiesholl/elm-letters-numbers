module Comparers exposing (..)

import Models exposing (Expression(..), Operator(..))

opPrecedence op =
  case op of
    Add -> 1
    Subtract -> 2

compareExpressions a b =
  case a of
    ConstExp c1 ->
      case b of
        ConstExp c2 ->
          compare c1 c2
        OpExp _ ->
          LT
    OpExp pairedOpA ->
      case b of
        ConstExp _ ->
          GT
        OpExp pairedOpB ->
          let
            sortLeft = compareExpressions pairedOpA.pair.left pairedOpB.pair.left
            sortRight = compareExpressions pairedOpA.pair.right pairedOpB.pair.right
            sortOp = compareOperators pairedOpA.op pairedOpB.op
          in
            case sortLeft of
              EQ ->
                case sortRight of
                  EQ ->
                    sortOp
                  _ -> sortRight
              _ -> sortLeft

compareOperators a b =
  let
    precA = opPrecedence a
    precB = opPrecedence b
  in
    compare precA precB
