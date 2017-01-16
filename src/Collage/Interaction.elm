module Collage.Interaction
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
        , Key(..)
        )

{-| Incremental introduction to interactive graphics programs.

# Draw
@docs Drawing, draw

# Draw with Time
@docs Animation, animate

# Draw with Time and Model
@docs Simulation, simulate

# Draw with Model and Messages
@docs Interaction, interact, Msg, Key
-}

import AnimationFrame
import Element
import Collage
import Html
import Html.Lazy exposing (lazy2)
import Keyboard exposing (KeyCode)
import Mouse
import Time exposing (Time)
import Window


{-| -}
type Msg
    = TimeTick Time
    | MouseClick { x : Int, y : Int }
    | MouseDown { x : Int, y : Int }
    | MouseUp { x : Int, y : Int }
    | MouseMove { x : Int, y : Int }
    | KeyDown Key
    | KeyUp Key
    | WindowResize { width : Int, height : Int }


{-| -}
type Key
    = Tab
    | Enter
    | Shift
    | Space
    | Left
    | Up
    | Right
    | Down
    | Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z
    | Other KeyCode


toKey : KeyCode -> Key
toKey keyCode =
    case keyCode of
        9 ->
            Tab

        13 ->
            Enter

        16 ->
            Shift

        32 ->
            Space

        37 ->
            Left

        38 ->
            Up

        39 ->
            Right

        40 ->
            Down

        48 ->
            Zero

        49 ->
            One

        50 ->
            Two

        51 ->
            Three

        52 ->
            Four

        53 ->
            Five

        54 ->
            Six

        55 ->
            Seven

        56 ->
            Eight

        57 ->
            Nine

        65 ->
            A

        66 ->
            B

        67 ->
            C

        68 ->
            D

        69 ->
            E

        70 ->
            F

        71 ->
            G

        72 ->
            H

        73 ->
            I

        74 ->
            J

        75 ->
            K

        76 ->
            L

        77 ->
            M

        78 ->
            N

        79 ->
            O

        80 ->
            P

        81 ->
            Q

        82 ->
            R

        83 ->
            S

        84 ->
            T

        85 ->
            U

        86 ->
            V

        87 ->
            W

        88 ->
            X

        89 ->
            Y

        90 ->
            Z

        _ ->
            Other keyCode


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


{-| Necessary for lazy! We need to define at the top level so that the
reference of the lazy fn is the same across calls.

From http://elm-lang.org/blog/blazing-fast-html:
So we just check to see if fn (viewAndModelToHtml) and args (view and model) are
the same as last frame by comparing the old and new values by reference. This is
super cheap, and if they are the same, the lazy function can often avoid a ton
of work. This is a pretty simple trick that can speed things up significantly.
-}
viewAndModelToHtml : (model -> Collage.Form) -> model -> Html.Html msg
viewAndModelToHtml view model =
    Collage.collage 500 500 [ view model ] |> Element.toHtml


{-| -}
draw : Collage.Form -> Drawing
draw view =
    Html.beginnerProgram
        { model = ()
        , view = \_ -> viewAndModelToHtml (\_ -> view) ()
        , update = \_ model -> model
        }


{-| -}
animate : (Time -> Collage.Form) -> Animation
animate view =
    Html.program
        { init = 0 ! []
        , view = \time -> viewAndModelToHtml view time
        , update = \(Diff diff) time -> (time + diff) ! []
        , subscriptions = \_ -> AnimationFrame.diffs Diff
        }


{-| -}
simulate :
    model
    -> (model -> Collage.Form)
    -> (Time -> model -> model)
    -> Simulation model
simulate init view update =
    Html.program
        { init = ( 0, init ) ! []
        , view = \( _, model ) -> lazy2 viewAndModelToHtml view model
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
    -> (model -> Collage.Form)
    -> (Msg -> model -> model)
    -> Interaction model
interact init view update =
    Html.program
        { init = ( 0, init ) ! []
        , view = \( _, model ) -> lazy2 viewAndModelToHtml view model
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
        [ AnimationFrame.diffs TimeTick
        , Mouse.clicks MouseClick
        , Mouse.downs MouseDown
        , Mouse.ups MouseUp
        , Mouse.moves MouseMove
        , Keyboard.downs (toKey >> KeyDown)
        , Keyboard.ups (toKey >> KeyUp)
        , Window.resizes WindowResize
        ]
