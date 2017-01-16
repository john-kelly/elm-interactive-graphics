module Animation exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Collage.Interaction exposing (animate, Animation)


view time =
    let
        seconds =
            time / 1000

        radius =
            50

        y =
            (sin seconds) * 50
    in
        move ( 0, y ) (filled red (circle radius))


main : Animation
main =
    animate view
