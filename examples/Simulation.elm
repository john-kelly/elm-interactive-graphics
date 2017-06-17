module Simulation exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Collage.Program exposing (simulationProgram, SimulationProgram)


type alias Model =
    ( Float, Float, Float, Float, Float )


init : Model
init =
    ( 0, 0, 2, 1, 30 )


view : Model -> Form
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


main : SimulationProgram Model
main =
    simulationProgram
        { init = init
        , view = view
        , update = update
        }
