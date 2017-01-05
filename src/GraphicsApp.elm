module GraphicsApp
    exposing
        ( draw
        , animate
        , simulate
        , interact
        , ClockTick
        , Event(..)
        )

{-| Incremental introduction to interactive graphics programs.

# Apps
@docs draw, animate, simulate, interact

# Messages
@docs ClockTick, Event
-}

import Element exposing (toHtml)
import Html
import AnimationFrame
import Time exposing (Time)
import Mouse


{- TODO
   - debug options:
       - various debug options: https://github.com/jcollard/elm-playground/blob/master/src/Playground.elm#L102
       - display grid + mouse position: https://github.com/MacCASOutreach/graphicsvg/blob/1.1.1/GraphicSVG.elm#L1042 and http://www.janis-voigtlaender.eu/Elm-Kurs/examples/Kreise.html
       - time travel: play + pause + scrub button! to allow for vizualization and debugging: http://package.elm-lang.org/packages/jinjor/elm-time-travel/latest + http://worrydream.com/LearnableProgramming/
   - lazy: https://github.com/evancz/elm-playground/blob/master/src/Playground.elm#L227
   - Computer/World: https://github.com/jcollard/elm-playground/blob/master/src/Playground/Input.elm#L25 and https://github.com/evancz/elm-playground/blob/master/src/Playground.elm#L44
   - remaning subscriptions
   - general program? and program with flags?
   - documentation
   - name for messages? ClockTick, Event? what are better names?
   - how to fix type signature of the Program? hidden float is there... Does it make sense to
       to make that opaque in some way?
-}


{-| -}
type ClockTick
    = Diff Time


{-| -}
type Event
    = Tick Time
    | MouseClick


{-| -}
draw : Element.Element -> Program Never () Never
draw view =
    Html.program
        { init = () ! []
        , view = \_ -> toHtml view
        , update = \_ model -> model ! []
        , subscriptions = \_ -> Sub.none
        }


{-| -}
animate : (Float -> Element.Element) -> Program Never Float ClockTick
animate view =
    Html.program
        { init = ( 0, Cmd.none )
        , view = \time -> toHtml (view time)
        , update = \(Diff diff) time -> (time + diff) ! []
        , subscriptions = \_ -> AnimationFrame.diffs Diff
        }


{-| -}
simulate :
    model
    -> (model -> Element.Element)
    -> (Float -> model -> model)
    -> Program Never ( Float, model ) ClockTick
simulate init view update =
    Html.program
        { init = ( ( 0, init ), Cmd.none )
        , view = \model -> toHtml (view (Tuple.second model))
        , update =
            \(Diff diff) ( time, model ) ->
                ( time + diff, update (time + diff) model ) ! []
        , subscriptions = \_ -> AnimationFrame.diffs Diff
        }


{-| -}
interact :
    model
    -> (model -> Element.Element)
    -> (Event -> model -> model)
    -> Program Never ( Float, model ) Event
interact init view update =
    Html.program
        { init = ( ( 0, init ), Cmd.none )
        , view = \model -> toHtml (view (Tuple.second model))
        , update =
            \msg ( time, model ) ->
                case msg of
                    Tick diff ->
                        ( time + diff, update (Tick (time + diff)) model ) ! []

                    _ ->
                        ( time, update msg model ) ! []
        , subscriptions =
            \_ ->
                Sub.batch
                    [ AnimationFrame.diffs Tick
                    , Mouse.clicks (\_ -> MouseClick)
                    ]
        }
