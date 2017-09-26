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
  generateAttemptsArgs inputs [Add, Subtract]

generateAttemptsArgs: Inputs -> List Operator -> List Expression
generateAttemptsArgs inputs args =
  case List.head inputs of
    Nothing ->
      []
    Just headInputs ->
      case List.tail inputs of
        Nothing ->
          [Constant headInputs]
        Just tailInputs ->
          let
            c = Constant headInputs
            nextExpressions = generateAttemptsArgs tailInputs args
          in
            [c] ++ (List.concatMap (eachArg args c) nextExpressions)

eachArg: List Operator -> Expression -> Expression -> List Expression
eachArg args e1 e2 =
  List.map (Pair e1 e2) args

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
