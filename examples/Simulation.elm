module Simulation exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Collage.Interaction exposing (simulate, Simulation)


type alias Model =
    ( Float, Float, Float, Float, Float )


model : Model
model =
    ( 0, 0, 2, 1, 30 )


view : Model -> Collage.Form
view ( x, y, dx, dy, radius ) =
    move ( x, y ) (filled red (circle radius))


update : Float -> Model -> Model
update time ( x, y, dx, dy, radius ) =
    if x + radius > 200 || x - radius < -200 then
        ( x - dx, y + dy, -dx, dy, radius )
    else if y + radius > 200 || y - radius < -200 then
        ( x + dx, y - dy, dx, -dy, radius )
    else
        ( x + dx, y + dy, dx, dy, radius )


main : Simulation Model
main =
    simulate model view update
