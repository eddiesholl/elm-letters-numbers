module Models exposing (..)

type alias Input = Maybe Int
type alias Inputs = List Input

inputInvalid inp =
  case inp of
    Nothing ->
      True
    _ ->
      False

hasInvalidInput ins =
  List.any inputInvalid ins

type alias Model =
  { inputs : Inputs
  }
