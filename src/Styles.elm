module Styles exposing (..)

import Html.Attributes
import Css exposing (..)
import Css.Elements exposing (body, li)
import Css.Namespace exposing (namespace)

styles =
    Css.asPairs >> Html.Attributes.style

type CssClasses
    = Page

flexColumn =
  [ displayFlex
  , flexDirection column
  ]

flexRow =
  [ displayFlex
  , flexDirection row
  ]

page =
  [ margin (px 20)
  ]

rowSpacer =
  [ minWidth (px 20)
  ]
