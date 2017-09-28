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
  case compare a.distance b.distance of
    EQ -> compare a.length b.length -- descending sort order
    a -> a


calculateResults: Input -> List Expression -> List Solution
calculateResults target attempts =
  List.map (calculateResult target) attempts

calculateResult: Input -> Expression -> Solution
calculateResult target e =
  let
    result = evalExpression e
  in
    { attempt = e, result = evalExpression e, distance = abs (target - result), length = expLength e }

expLength: Expression -> Int
expLength e =
  case e of
    ConstExp _ ->
      1
    OpExp opExp ->
      (expLength opExp.pair.left) + (expLength opExp.pair.right)

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
      Multiply ->
        eval1 * eval2
      -- Divide ->
      --   eval1 / eval2
