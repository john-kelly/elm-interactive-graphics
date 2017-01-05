module GraphicsApp
    exposing
        ( Drawing
        , draw
        , Animation
        , animate
        , Simulation
        , simulate
        , Interaction
        , interact
        , Event(..)
        )

{-| Incremental introduction to interactive graphics programs.

# Draw
@docs Drawing, draw

# Draw with Time
@docs Animation, animate

# Draw with Time and State
@docs Simulation, simulate

# Draw with State and Events
@docs Interaction, interact, Event
-}

import AnimationFrame
import Element exposing (toHtml)
import Html
import Mouse
import Time exposing (Time)


{- TODO
   - lazy: https://github.com/evancz/elm-playground/blob/master/src/Playground.elm#L227
   - add remaining subscriptions
   - Computer/World: https://github.com/jcollard/elm-playground/blob/master/src/Playground/Input.elm#L25 and https://github.com/evancz/elm-playground/blob/master/src/Playground.elm#L44
   - think about names for Tick, Event, Interaction
   - using word model vs state?
   - documentation
   - general program to support Cmds and/or custom subscriptions?
   - debug options:
       - various debug options: https://github.com/jcollard/elm-playground/blob/master/src/Playground.elm#L102
       - display grid + mouse position: https://github.com/MacCASOutreach/graphicsvg/blob/1.1.1/GraphicSVG.elm#L1042 and http://www.janis-voigtlaender.eu/Elm-Kurs/examples/Kreise.html
       - time travel: play + pause + scrub button! to allow for vizualization and debugging: http://package.elm-lang.org/packages/jinjor/elm-time-travel/latest + http://worrydream.com/LearnableProgramming/
-}
{- NOTE
   - When we get to Interaction apps, we learn that Time is just an Event
-}


type Tick
    = Diff Time


{-| -}
type alias Drawing =
    Program Never () Never


{-| -}
type alias Animation =
    Program Never Time Tick


{-| -}
type alias Simulation state =
    Program Never ( Time, state ) Tick


{-| -}
type alias Interaction state =
    Program Never ( Time, state ) Event


{-| -}
type Event
    = Tick Time
    | MouseClick


{-| -}
draw : Element.Element -> Drawing
draw view =
    Html.program
        { init = () ! []
        , view = \_ -> toHtml view
        , update = \_ model -> model ! []
        , subscriptions = \_ -> Sub.none
        }


{-| -}
animate : (Float -> Element.Element) -> Animation
animate view =
    Html.program
        { init = ( 0, Cmd.none )
        , view = \time -> toHtml (view time)
        , update = \(Diff diff) time -> (time + diff) ! []
        , subscriptions = \_ -> AnimationFrame.diffs Diff
        }


{-| -}
simulate :
    state
    -> (state -> Element.Element)
    -> (Float -> state -> state)
    -> Simulation state
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
    state
    -> (state -> Element.Element)
    -> (Event -> state -> state)
    -> Interaction state
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
