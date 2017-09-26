-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Models exposing (Problem, Model, Input, Inputs, Solution, SolutionSet, SolverState(..), anyInvalidInput, isTargetValid, isProblemReady)
import Solver exposing (solve)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


init : (Model, Cmd Msg)
init =
  ( Model (Problem [] Nothing) Nothing Waiting
  , Cmd.none
  )



-- UPDATE

type Msg
  = UpdateInput Int Input
  | AddInput
  | InvalidInput
  | UpdateTarget Input
  | StartSolver

alterInput : Inputs -> Int -> Input -> Inputs
alterInput inputs ix newVal =
  case (List.head inputs) of
    Nothing ->
      inputs
    Just h ->
      case (List.tail inputs) of
        Nothing ->
          [h]
        Just t ->
          case ix of
            0 ->
              newVal :: t
            _ ->
              h :: (alterInput t (ix - 1) newVal)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateInput ix newVal ->
      let
        oldProblem = model.problem
        newProblem = { oldProblem | inputs = (alterInput oldProblem.inputs ix newVal) }
      in
        ({ model | problem = newProblem }, Cmd.none)
    AddInput ->
      let
        oldProblem = model.problem
        newProblem = { oldProblem | inputs = oldProblem.inputs ++ [Nothing] }
      in
        ({ model | problem = newProblem }, Cmd.none)
    UpdateTarget newTarget ->
      let
        oldProblem = model.problem
        newProblem = { oldProblem | target = newTarget }
      in
        ({ model | problem = newProblem}, Cmd.none)
    InvalidInput ->
      (model, Cmd.none)
    StartSolver ->
      ({ model | solution = Just (solve model.problem), state = Running }, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "Hi"]
    , div [] [(inputsView model.problem)]
    , solverView model
    ]

inputTyped : Int -> String -> Msg
inputTyped ix input =
  case String.toInt input of
    Ok n ->
      UpdateInput ix (Just n)
    Err _ ->
      InvalidInput

targetTyped : String -> Msg
targetTyped input =
  case String.toInt input of
    Ok n ->
      UpdateTarget (Just n)
    Err _ ->
      InvalidInput


inputDisplay inp =
  case inp of
    Just i ->
      toString i
    Nothing ->
      ""

inputView : Int -> Input -> Html Msg
inputView ix inp =
  div [] [
    input [onInput (inputTyped ix), inp |> inputDisplay |> value] []
    ]

inputsView : Problem -> Html Msg
inputsView problem =
  let
    invalidInput = anyInvalidInput problem.inputs
    validTarget = isTargetValid problem.target
  in
    div []
      [ h3 [] [text "Target"]
      , input [onInput targetTyped, problem.target |> inputDisplay |> value] []
      , h3 [] [text "Inputs"]
      , div [] (List.indexedMap inputView problem.inputs)
      , button [onClick AddInput, disabled invalidInput] [text "add"]
      ]

textDiv t =
    t |> toString |> text

solutionView: Solution -> Html Msg
solutionView solution =
  div []
    [ textDiv solution.attempt
    , textDiv solution.result
    ]

solverView: Model -> Html Msg
solverView { problem, solution, state } =
  let
    canStart = case state of
      Waiting ->
        isProblemReady problem
      _ ->
        False
  in
    div []
      [ h3 [] [text "Solver"]
      , button [onClick StartSolver, canStart |> not |> disabled] [text "Start"]
      , solutionsView solution
      ]

solutionsView: Maybe SolutionSet -> Html Msg
solutionsView solution =
  case solution of
    Nothing ->
      text "No solution yet"
    Just s ->
      div []
        [ text (s.solutions |> List.length |> toString) ++ " solutions"
        , div [] (List.map solutionView s.solutions)
        ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
