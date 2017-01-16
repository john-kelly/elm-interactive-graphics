module Drawing exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Collage.Interaction exposing (draw, Drawing)


view =
    filled red (circle 50)


main : Drawing
main =
    draw view
