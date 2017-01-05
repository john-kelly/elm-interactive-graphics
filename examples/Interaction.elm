module Interaction exposing (..)

import GraphicsApp exposing (interact, Interaction, Msg(..))
import Color exposing (..)
import Collage exposing (..)


model =
    ( 0, 0, 2, 1, 30 )


view ( x, y, dx, dy, radius ) =
    collage 400 400 [ move ( x, y ) (filled red (circle radius)) ]


update msg ( x, y, dx, dy, radius ) =
    case msg of
        TimeTick _ ->
            if x + radius > 200 || x - radius < -200 then
                ( x - dx, y + dy, -dx, dy, radius )
            else if y + radius > 200 || y - radius < -200 then
                ( x + dx, y - dy, dx, -dy, radius )
            else
                ( x + dx, y + dy, dx, dy, radius )

        MouseClick ->
            ( x - dx, y + dy, -dx, dy, radius )


main : Interaction ( Float, Float, Float, Float, Float )
main =
    interact model view update
