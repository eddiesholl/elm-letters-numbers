module Solver exposing (..)

import Models exposing (Input, Inputs)

type alias Problem =
  { inputs: Inputs
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

solve: Problem -> SolutionSet
solve problem =
    { problem, problem.inputs |> generateAttempts |> calculateResults }

generateAttempts: Inputs -> List Expression
generateAttempts inputs =
  []

calculateResults: List Expression -> List Solution
calculateResults attempts =
  List.map evaluateExpression attempts

calculateResult: Expression -> Solution
calculateResult e =
  { e, evaluateExpression e }

evaluateExpression: Expression -> Int
evaluateExpression e =
  0
