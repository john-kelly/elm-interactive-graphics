# elm-interactive-graphics

Gradual introduction to interactive graphics programs.

#### The Steps:
1. Drawing: draw elements
2. Animation: draw with time
3. Simulation: draw with time and model
4. Interaction: draw with time and msgs

## Sample Simulation Program
```elm
import Color exposing (..)
import Collage exposing (Form)
import Collage.Interaction exposing (simulationProgram, SimulationProgram)


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


main : Simulation Model
main =
    simulationProgram
        { init = init
        , view = view
        , update = update
        }
```

# Influences:
- https://github.com/jcollard/elm-playground
- https://github.com/evancz/elm-playground
- https://github.com/MacCASOutreach/graphicsvg/tree/1.1.1
- https://github.com/jvoigtlaender/Elm-Kurs/blob/master/src/lib/Lib.elm
- https://github.com/google/codeworld
