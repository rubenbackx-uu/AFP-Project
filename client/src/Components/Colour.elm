module Components.Colour exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)

type alias ColourScheme =
    { primaryColour : Color
    , primaryColourActive : Color
    , secondaryColour : Color
    , backgroundColour : Color
    , headerColour : Color
    , textColour : Color
    , borderColour : Color
    }

scheme : ColourScheme
scheme = dark

dark : ColourScheme
dark = {
    primaryColour = hex "#E0A000",
    primaryColourActive = hex "#C09000",
    secondaryColour = hex "#4010D0",
    backgroundColour = hex "#282828",
    headerColour = hex "#141414",
    textColour = hex "#FFFFFF",
    borderColour = hex "#808080"
    }