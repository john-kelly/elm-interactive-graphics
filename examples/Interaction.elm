module Interaction exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Collage.Program exposing (interactiveProgram, InteractiveProgram, Msg(..))


type alias Model =
    ( Float, Float, Float, Float, Float )


init : Model
init =
    ( 0, 0, 2, 1, 30 )


view : Model -> Form
view ( x, y, dx, dy, radius ) =
    move ( x, y ) (filled red (circle radius))


update : Msg -> Model -> Model
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


main : InteractiveProgram Model
main =
    interactiveProgram
        { init = init
        , view = view
        , update = update
        }
