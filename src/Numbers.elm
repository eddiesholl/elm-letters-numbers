-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Models exposing (Model, Input, Inputs, hasInvalidInput)

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
  ( Model []
  , Cmd.none
  )



-- UPDATE


type Msg
  = UpdateInput Int Input
  | AddInput
  | InvalidInput

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
      ({ model | inputs = (alterInput model.inputs ix newVal) }, Cmd.none)
    AddInput ->
      ({ model | inputs = model.inputs ++ [Nothing]}, Cmd.none)
    InvalidInput ->
      (model, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "Hi"]
    , div [] [(inputsView model)]
    ]

inputTyped : Int -> String -> Msg
inputTyped ix input =
  case String.toInt input of
    Ok n ->
      UpdateInput ix (Just n)
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

inputsView : Model -> Html Msg
inputsView model =
  let
    disableAdd = hasInvalidInput model.inputs
  in
    div []
      [ div [] (List.indexedMap inputView model.inputs)
      , button [onClick AddInput, disabled disableAdd] [text "add"]
      ]

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
