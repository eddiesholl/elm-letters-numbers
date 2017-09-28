module Solver exposing (..)

import Models exposing (Input, Inputs, Problem(..), Solution, SolutionSet, Expression(..), Operator(..))
import Attempts exposing (generateAttempts)

solve: Problem -> Maybe SolutionSet
solve problem =
  case problem of
    InvalidProblem ->
      Just { problem = problem, solutions = [-1] |> generateAttempts |> (calculateResults 0) }
    ValidProblem inputs target ->
      Just { problem = problem, solutions = inputs |> generateAttempts |> (calculateResults target) |> sortResults }

sortResults solutions =
  List.sortWith compareResults solutions

compareResults a b =
  compare a.distance b.distance

calculateResults: Input -> List Expression -> List Solution
calculateResults target attempts =
  List.map (calculateResult target) attempts

calculateResult: Input -> Expression -> Solution
calculateResult target e =
  let
    result = evalExpression e
  in
    { attempt = e, result = evalExpression e, distance = abs (target - result), length = 0 }

evalExpression: Expression -> Int
evalExpression e =
  case e of
    ConstExp c ->
      c
    OpExp opExp ->
      evalPair opExp.pair.left opExp.pair.right opExp.op

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
