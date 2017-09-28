module Test.Attempts exposing (..)

import Expect exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import ElmTestBDDStyle exposing (..)
import Dict

import Attempts exposing (..)
import Models exposing (Expression(..), Operator(..), PairedOperator, Pair)
import Comparers exposing (compareExpressions)

c1 = ConstExp 1
c2 = ConstExp 2
c3 = ConstExp 3
p12Add = OpExp (PairedOperator (Pair c1 c2) Add)
p21Add = OpExp (PairedOperator (Pair c2 c1) Add)
p12Subtract = OpExp (PairedOperator (Pair c1 c2) Subtract)
p21Subtract = OpExp (PairedOperator (Pair c2 c1) Subtract)

haveAllItems: List a -> List a -> (a -> a -> Order) -> Expectation
haveAllItems l1 l2 comparer =
  expect (List.sortWith comparer l1) to equalLists (List.sortWith comparer l2)

haveAllExpressions: List Expression -> List Expression -> Expectation
haveAllExpressions l1 l2 =
  haveAllItems l1 l2 compareExpressions


suite : Test
suite =
  describe "Attempts"
    [ describe "generateAttempts"
      [ it "processes empty input" <|
        expect (generateAttempts []) to equal []
      ]
      , describe "inputsToConstants"
      [ it "processes empty input" <|
        expect (inputsToConstants []) to equal []
      , it "takes a single input" <|
        expect (inputsToConstants [1]) to haveAllExpressions [c1]
      , it "takes 3 inputs" <|
        expect (inputsToConstants [1, 2, 3]) to haveAllExpressions [c1, c2, c3]
      ]
      , describe "deriveExpressions"
      [ it "processes empty input" <|
        expect (deriveExpressions [] []) to equal []
      , it "takes 2 inputs and a single operator" <|
        expect (deriveExpressions [Add] [c1, c2]) to haveAllExpressions [c1, c2, p12Add]
      , it "takes 2 inputs and 2 operators" <|
        expect (deriveExpressions [Add, Subtract] [c1, c2] ) to haveAllExpressions [c1, c2, p12Add, p12Subtract, p21Subtract]
      ]
    ]
