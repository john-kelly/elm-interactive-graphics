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
import Html.Lazy exposing (lazy)
import Keyboard exposing (KeyCode)
import Mouse exposing (Position)
import Time exposing (Time)
import Window exposing (Size)


{-| -}
type Msg
    = TimeTick Time
    | MouseClick
    | MouseDown
    | MouseUp
    | MouseMove Position
    | KeyDown KeyCode
    | KeyUp KeyCode
    | KeyPress KeyCode
    | WindowResize Size


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
    Html.program
        { init = () ! []
        , view = \_ -> Element.toHtml view
        , update = \_ model -> model ! []
        , subscriptions = \_ -> Sub.none
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


{-| -}
simulate :
    model
    -> (model -> Element.Element)
    -> (Time -> model -> model)
    -> Simulation model
simulate init view update =
    let
        -- necessary for lazy?
        viewToHtml =
            view >> Element.toHtml
    in
        Html.program
            { init = ( 0, init ) ! []
            , view = \( _, model ) -> lazy viewToHtml model
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
        updatedTime =
            time + diff

        newModel =
            update updatedTime model

        updatedModel =
            -- necessary for lazy?
            if newModel == model then
                model
            else
                newModel
    in
        ( updatedTime, updatedModel ) ! []


{-| -}
interact :
    model
    -> (model -> Element.Element)
    -> (Msg -> model -> model)
    -> Interaction model
interact init view update =
    let
        -- necessary for lazy?
        viewToHtml =
            view >> Element.toHtml
    in
        Html.program
            { init = ( 0, init ) ! []
            , view = \( _, model ) -> lazy viewToHtml model
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
                updatedTime =
                    time + diff

                newModel =
                    update (TimeTick updatedTime) model

                updatedModel =
                    -- necessary for lazy?
                    if newModel == model then
                        model
                    else
                        newModel
            in
                ( updatedTime, updatedModel ) ! []

        _ ->
            let
                newModel =
                    update msg model

                updatedModel =
                    -- necessary for lazy?
                    if newModel == model then
                        model
                    else
                        newModel
            in
                ( time, updatedModel ) ! []


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
