module Animation exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Collage.Program exposing (animationProgram, AnimationProgram)


view : Float -> Form
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


main : AnimationProgram
main =
    animationProgram view
