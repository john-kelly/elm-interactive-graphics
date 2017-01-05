module Main exposing (..)

import GraphicsApp exposing (..)
import Color exposing (..)
import Collage exposing (..)


-- Drawing


view1 =
    collage 400 400 [ filled red (circle 50) ]


main =
    draw view1



-- Animation


view2 time =
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


main2 =
    animate view2



-- Simulation


model3 =
    ( 0, 0, 2, 1, 30 )


view3 ( x, y, dx, dy, radius ) =
    collage 400 400 [ move ( x, y ) (filled red (circle radius)) ]


update3 time ( x, y, dx, dy, radius ) =
    if x + radius > 200 || x - radius < -200 then
        ( x - dx, y + dy, -dx, dy, radius )
    else if y + radius > 200 || y - radius < -200 then
        ( x + dx, y - dy, dx, -dy, radius )
    else
        ( x + dx, y + dy, dx, dy, radius )


main3 =
    simulate model3 view3 update3



-- Interactive


model4 =
    ( 0, 0, 2, 1, 30 )


view4 ( x, y, dx, dy, radius ) =
    collage 400 400 [ move ( x, y ) (filled red (circle radius)) ]


update4 msg ( x, y, dx, dy, radius ) =
    case msg of
        Tick _ ->
            if x + radius > 200 || x - radius < -200 then
                ( x - dx, y + dy, -dx, dy, radius )
            else if y + radius > 200 || y - radius < -200 then
                ( x + dx, y - dy, dx, -dy, radius )
            else
                ( x + dx, y + dy, dx, dy, radius )

        MouseClick ->
            ( x - dx, y + dy, -dx, dy, radius )


main4 =
    interact model4 view4 update4
