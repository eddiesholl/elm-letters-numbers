module Models exposing (..)

type alias Input = Maybe Int
type alias Inputs = List Input

inputInvalid inp =
  case inp of
    Nothing ->
      True
    _ ->
      False

anyInvalidInput ins =
  List.any inputInvalid ins

isTargetValid t =
  case t of
    Nothing ->
      False
    _ ->
      True

isProblemReady { inputs, target } =
  not (anyInvalidInput inputs) && isTargetValid target && ((List.length inputs) > 0)

type SolverState
  = Waiting
  | Running
  | Done

type alias Model =
  { problem : Problem
  , solution : Maybe SolutionSet
  , state : SolverState
  }

type alias Problem =
  { inputs: Inputs
  , target: Input
  }

type alias SolutionSet =
  { problem: Problem
  , solutions: Solutions
  }

type alias Solution =
  { attempt: Expression
  , result: Int
  }

type alias Solutions = List Solution

type Expression
 = Constant Int
 | Pair Expression Expression Operator

type Operator
 = Add
 | Substract
