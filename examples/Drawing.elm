module Drawing exposing (..)

import GraphicsApp exposing (draw, Drawing)
import Color exposing (..)
import Collage exposing (..)


view =
    collage 400 400 [ filled red (circle 50) ]


main : Drawing
main =
    draw view
