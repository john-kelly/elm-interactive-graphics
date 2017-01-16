module Interaction exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Collage.Interaction exposing (interact, Interaction, Msg(..))


model =
    ( 0, 0, 2, 1, 30 )


view ( x, y, dx, dy, radius ) =
    move ( x, y ) (filled red (circle radius))


update msg (( x, y, dx, dy, radius ) as model) =
    case msg of
        TimeTick _ ->
            if x + radius > 200 || x - radius < -200 then
                ( x - dx, y + dy, -dx, dy, radius )
            else if y + radius > 200 || y - radius < -200 then
                ( x + dx, y - dy, dx, -dy, radius )
            else
                ( x + dx, y + dy, dx, dy, radius )

        MouseClick _ ->
            ( x, y, -dx, dy, radius )

        _ ->
            model


main : Interaction ( Float, Float, Float, Float, Float )
main =
    interact model view update
