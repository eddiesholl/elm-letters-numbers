module Test.Attempts exposing (..)

import Expect exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import ElmTestBDDStyle exposing (..)
import Dict

import Attempts exposing (..)
import Models exposing (Expression(..), Operator(..))
import Comparers exposing (compareExpressions)

c1 = Constant 1
c2 = Constant 2
c3 = Constant 3
p12Add = Pair c1 c2 Add
p21Add = Pair c2 c1 Add
p12Subtract = Pair c1 c2 Subtract
p21Subtract = Pair c2 c1 Subtract

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
      , describe "generateAttemptsConsts"
      [ it "processes empty input" <|
        expect (generateAttemptsConsts []) to equal []
      , it "takes a single input" <|
        expect (generateAttemptsConsts [1]) to haveAllExpressions [Constant 1]
      , it "takes 3 inputs" <|
        expect (generateAttemptsConsts [1, 2, 3]) to haveAllExpressions [c1, c2, c3]
      ]
      , describe "generateAttemptsArgs"
      [ it "processes empty input" <|
        expect (generateAttemptsArgs [] []) to equal []
      , it "takes 2 inputs and a single operator" <|
        expect (generateAttemptsArgs [1, 2] [Add]) to haveAllExpressions [p12Add]
      , it "takes 2 inputs and 2 operators" <|
        expect (generateAttemptsArgs [1, 2] [Add, Subtract]) to haveAllExpressions [p12Add, p12Subtract, p21Add, p21Subtract]
      ]
    ]
