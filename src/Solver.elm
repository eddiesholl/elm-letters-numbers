module Solver exposing (..)

import Models exposing (Input, Inputs, Problem(..), Solution, SolutionSet, Expression(..), Operator(..))
import Attempts exposing (generateAttempts)

solve: Problem -> Maybe SolutionSet
solve problem =
  case problem of
    InvalidProblem -> Nothing
    ValidProblem inputs target ->
      Just { problem = problem, solutions = inputs |> generateAttempts |> calculateResults }

calculateResults: List Expression -> List Solution
calculateResults attempts =
  List.map calculateResult attempts

calculateResult: Expression -> Solution
calculateResult e =
  { attempt = e, result = evalExpression e }

evalExpression: Expression -> Int
evalExpression e =
  case e of
    Constant c ->
      c
    Pair e1 e2 op ->
      evalPair e1 e2 op

evalPair e1 e2 op =
  let
    eval1 = evalExpression e1
    eval2 = evalExpression e2
  in
    case op of
      Subtract ->
        eval1 - eval2
      Add ->
        eval1 + eval2
