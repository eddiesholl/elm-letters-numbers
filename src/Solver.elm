module Solver exposing (..)

import Models exposing (Input, Inputs, Problem(..), Solution, SolutionSet, Expression(..), Operator(..))

solve: Problem -> Maybe SolutionSet
solve problem =
  case problem of
    InvalidProblem -> Nothing
    ValidProblem inputs target ->
      Just { problem = problem, solutions = inputs |> generateAttempts |> calculateResults }

generateAttempts: Inputs -> List Expression
generateAttempts inputs =
  generateAttemptsArgs inputs [Add]

generateAttemptsArgs: Inputs -> List Operator -> List Expression
generateAttemptsArgs inputs args =
  case List.head inputs of
    Nothing ->
      []
    Just ih ->
      [Constant ih]


calculateResults: List Expression -> List Solution
calculateResults attempts =
  List.map calculateResult attempts

calculateResult: Expression -> Solution
calculateResult e =
  { attempt = e, result = evaluateExpression e }

evaluateExpression: Expression -> Int
evaluateExpression e =
  case e of
    Constant c ->
      c
    Pair e1 e2 op ->
      0
