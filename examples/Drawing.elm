module Drawing exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Collage.Program exposing (draw, Drawing)


view : Form
view =
    filled red (circle 50)


main : Drawing
main =
    draw view
