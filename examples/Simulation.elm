module Simulation exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Collage.Interaction exposing (simulate, Simulation)


model =
    ( 0, 0, 2, 1, 30 )


view ( x, y, dx, dy, radius ) =
    collage 400 400 [ move ( x, y ) (filled red (circle radius)) ]


update time ( x, y, dx, dy, radius ) =
    if x + radius > 200 || x - radius < -200 then
        ( x - dx, y + dy, -dx, dy, radius )
    else if y + radius > 200 || y - radius < -200 then
        ( x + dx, y - dy, dx, -dy, radius )
    else
        ( x + dx, y + dy, dx, dy, radius )


main : Simulation ( Float, Float, Float, Float, Float )
main =
    simulate model view update
