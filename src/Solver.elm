module Solver exposing (..)

import Models exposing (Input, Inputs, Problem, Solution, SolutionSet, Expression)

solve: Problem -> SolutionSet
solve problem =
    { problem = problem, solutions = problem.inputs |> generateAttempts |> calculateResults }

generateAttempts: Inputs -> List Expression
generateAttempts inputs =
  []

calculateResults: List Expression -> List Solution
calculateResults attempts =
  List.map calculateResult attempts

calculateResult: Expression -> Solution
calculateResult e =
  { attempt = e, result = evaluateExpression e }

evaluateExpression: Expression -> Int
evaluateExpression e =
  0
