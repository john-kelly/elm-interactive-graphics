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
        , Msg(..)
        )

{-| Incremental introduction to interactive graphics programs.

# Draw
@docs Drawing, draw

# Draw with Time
@docs Animation, animate

# Draw with Time and Model
@docs Simulation, simulate

# Draw with Model and Messages
@docs Interaction, interact, Msg
-}

import AnimationFrame
import Element
import Html
import Html.Lazy exposing (lazy2)
import Keyboard exposing (KeyCode)
import Mouse
import Time exposing (Time)
import Window


{-| -}
type Msg
    = TimeTick Time
    | MouseClick
    | MouseDown
    | MouseUp
    | MouseMove { x : Int, y : Int }
    | KeyDown KeyCode
    | KeyUp KeyCode
    | KeyPress KeyCode
    | WindowResize { width : Int, height : Int }


type TimeMsg
    = Diff Time


{-| -}
type alias Drawing =
    Program Never () Never


{-| -}
type alias Animation =
    Program Never Time TimeMsg


{-| -}
type alias Simulation model =
    Program Never ( Time, model ) TimeMsg


{-| -}
type alias Interaction model =
    Program Never ( Time, model ) Msg


{-| -}
draw : Element.Element -> Drawing
draw view =
    Html.beginnerProgram
        { model = ()
        , view = \_ -> Element.toHtml view
        , update = \_ model -> model
        }


{-| -}
animate : (Time -> Element.Element) -> Animation
animate view =
    Html.program
        { init = 0 ! []
        , view = \time -> Element.toHtml (view time)
        , update = \(Diff diff) time -> (time + diff) ! []
        , subscriptions = \_ -> AnimationFrame.diffs Diff
        }


{-| Necessary for lazy! We need to define at the top level so that the
reference of the lazy fn is the same across calls.

From http://elm-lang.org/blog/blazing-fast-html:
So we just check to see if fn (viewToHtml) and args (view and model) are the
same as last frame by comparing the old and new values by reference. This is
super cheap, and if they are the same, the lazy function can often avoid a ton
of work. This is a pretty simple trick that can speed things up significantly.
-}
viewToHtml : (model -> Element.Element) -> model -> Html.Html msg
viewToHtml view model =
    Element.toHtml (view model)


{-| -}
simulate :
    model
    -> (model -> Element.Element)
    -> (Time -> model -> model)
    -> Simulation model
simulate init view update =
    Html.program
        { init = ( 0, init ) ! []
        , view = \( _, model ) -> lazy2 viewToHtml view model
        , update = simulationUpdate update
        , subscriptions = \_ -> AnimationFrame.diffs Diff
        }


simulationUpdate :
    (Time -> model -> model)
    -> TimeMsg
    -> ( Time, model )
    -> ( ( Time, model ), Cmd TimeMsg )
simulationUpdate update (Diff diff) ( time, model ) =
    let
        newTime =
            time + diff

        updatedModel =
            update newTime model

        newModel =
            -- necessary for lazy
            if updatedModel == model then
                model
            else
                updatedModel
    in
        ( newTime, newModel ) ! []


{-| -}
interact :
    model
    -> (model -> Element.Element)
    -> (Msg -> model -> model)
    -> Interaction model
interact init view update =
    Html.program
        { init = ( 0, init ) ! []
        , view = \( _, model ) -> lazy2 viewToHtml view model
        , update = interactionUpdate update
        , subscriptions = interactionSubscriptions
        }


interactionUpdate :
    (Msg -> model -> model)
    -> Msg
    -> ( Time, model )
    -> ( ( Time, model ), Cmd Msg )
interactionUpdate update msg ( time, model ) =
    case msg of
        TimeTick diff ->
            let
                newTime =
                    time + diff

                updatedModel =
                    update (TimeTick newTime) model

                newModel =
                    -- necessary for lazy
                    if updatedModel == model then
                        model
                    else
                        updatedModel
            in
                ( newTime, newModel ) ! []

        _ ->
            let
                updatedModel =
                    update msg model

                newModel =
                    -- necessary for lazy
                    if updatedModel == model then
                        model
                    else
                        updatedModel
            in
                ( time, newModel ) ! []


interactionSubscriptions : ( Time, model ) -> Sub Msg
interactionSubscriptions _ =
    Sub.batch
        [ Window.resizes WindowResize
        , Mouse.clicks (\_ -> MouseClick)
        , Mouse.downs (\_ -> MouseDown)
        , Mouse.ups (\_ -> MouseUp)
        , Mouse.moves MouseMove
        , AnimationFrame.diffs TimeTick
        , Keyboard.presses KeyPress
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
