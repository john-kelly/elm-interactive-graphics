module Drawing exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Collage.Program exposing (drawingProgram, DrawingProgram)


view : Form
view =
    filled red (circle 50)


main : DrawingProgram
main =
    drawingProgram view
