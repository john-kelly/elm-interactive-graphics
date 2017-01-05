module Animation exposing (..)

import GraphicsApp exposing (animate, Animation)
import Color exposing (..)
import Collage exposing (..)


view time =
    let
        seconds =
            time / 1000

        radius =
            50

        x =
            50

        y =
            (sin seconds) * 50
    in
        collage 400 400 [ move ( x, y ) (filled red (circle radius)) ]


main : Animation
main =
    animate view
