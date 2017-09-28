module Models exposing (..)

type alias UserInput = Maybe Int
type alias UserInputs = List UserInput

type alias Input = Int
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

problemSetupToProblem: ProblemSetup -> Problem
problemSetupToProblem ps =
  case isProblemReady ps of
    True ->
      ValidProblem (List.map (Maybe.withDefault 0) ps.inputs) (Maybe.withDefault 0 ps.target)
    _ ->
      InvalidProblem

type SolverState
  = Waiting
  | Running
  | Done

type Msg
  = UpdateInput Int UserInput
  | AddInput
  | InvalidInput
  | UpdateTarget UserInput
  | StartSolver

type alias Model =
  { problem : ProblemSetup
  , solution : Maybe SolutionSet
  , state : SolverState
  }

type alias ProblemSetup =
  { inputs: UserInputs
  , target: UserInput
  }

type Problem =
  ValidProblem Inputs Input
  | InvalidProblem

type alias SolutionSet =
  { problem: Problem
  , solutions: Solutions
  }

type alias Solution =
  { attempt: Expression
  , result: Int
  , distance: Int
  , length: Int
  }

type alias Solutions = List Solution

type alias Pair =
  { left: Expression
  , right: Expression
  }

type alias PairedOperator =
  { pair: Pair
  , op: Operator
  }

type Expression
 = ConstExp Int
 | OpExp PairedOperator

type Operator
 = Add
 | Subtract
 | Multiply
 -- | Divide
