module Drawing exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Collage.Interaction exposing (draw, Drawing)


view =
    collage 400 400 [ filled red (circle 50) ]


main : Drawing
main =
    draw view
