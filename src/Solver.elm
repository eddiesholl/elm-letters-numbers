module Solver exposing (..)

import Models exposing (Input, Inputs)

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
