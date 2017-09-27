module Test.Attempts exposing (..)

import Expect exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import ElmTestBDDStyle exposing (..)
import Dict

import Attempts exposing (..)
import Models exposing (Expression(..), Operator(..))

operatorPrecedence op =
  case op of
    Add -> 1
    Subtract -> 2

c1 = Constant 1
c2 = Constant 2
c3 = Constant 3
p12Add = Pair c1 c2 Add
p12Subtract = Pair c1 c2 Subtract

haveAllItems: List a -> List a -> (a -> a -> Order) -> Expectation
haveAllItems l1 l2 comparer =
  expect (List.sortWith comparer l1) to equalLists (List.sortWith comparer l2)

haveAllExpressions: List Expression -> List Expression -> Expectation
haveAllExpressions l1 l2 =
  haveAllItems l1 l2 sortExpressions

sortExpressions a b =
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
            sortE1 = sortExpressions e1a e1b
            sortE2 = sortExpressions e2a e2b
            sortOp = sortOperators opa opb
          in
            case sortE1 of
              EQ ->
                case sortE2 of
                  EQ ->
                    sortOp
                  _ -> sortE2
              _ -> sortE1

sortOperators a b =
  let
    precA = operatorPrecedence a
    precB = operatorPrecedence b
  in
    compare precA precB

suite : Test
suite =
  describe "Attempts"
    [ describe "generateAttempts"
      [ it "processes empty input" <|
        expect (generateAttempts []) to equal []
      ]
      , describe "generateAttemptsArgs"
      [ it "processes empty input" <|
        expect (generateAttemptsArgs [] []) to equal []
      , it "takes a single input" <|
        expect (generateAttemptsArgs [1] []) to haveAllExpressions [Constant 1]
      , it "takes 3 inputs" <|
        expect (generateAttemptsArgs [1, 2, 3] []) to haveAllExpressions [c1, c2, c3]
      , it "takes 2 inputs and a single operator" <|
        expect (generateAttemptsArgs [1, 2] [Add]) to haveAllExpressions [Constant 1, Constant 2, p12Add]
      ]
    ]
