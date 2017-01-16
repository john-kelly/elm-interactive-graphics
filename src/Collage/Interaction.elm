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
import Html.Lazy exposing (lazy3)
import Keyboard exposing (KeyCode)
import Mouse
import Task
import Time exposing (Time)
import Window exposing (Size)


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


type alias Model model =
    { size : Size
    , time : Time
    , model : model
    }


{-| -}
type alias Drawing =
    Program Never (Model ()) Msg


{-| -}
type alias Animation =
    Program Never (Model ()) Msg


{-| -}
type alias Simulation model =
    Program Never (Model model) Msg


{-| -}
type alias Interaction model =
    Program Never (Model model) Msg


{-| -}
draw : Collage.Form -> Drawing
draw view =
    Html.program
        { init = init () WindowResize
        , view = \{ size } -> viewModelSizeToHtml (\_ -> view) () size
        , update = drawUpdate
        , subscriptions = \_ -> Window.resizes WindowResize
        }


drawUpdate msg model =
    case msg of
        WindowResize newSize ->
            { model | size = newSize } ! []

        _ ->
            model ! []


{-| -}
animate : (Time -> Collage.Form) -> Animation
animate view =
    Html.program
        { init = init () WindowResize
        , view = \{ time, size } -> viewModelSizeToHtml view time size
        , update = animateUpdate
        , subscriptions = animateSubs
        }


animateUpdate msg model =
    case msg of
        TimeTick newTime ->
            { model | time = newTime } ! []

        WindowResize newSize ->
            { model | size = newSize } ! []

        _ ->
            model ! []


animateSubs { time } =
    Sub.batch
        [ accumTimeSub time TimeTick
        , Window.resizes WindowResize
        ]


{-| -}
simulate :
    model
    -> (model -> Collage.Form)
    -> (Time -> model -> model)
    -> Simulation model
simulate start view update =
    Html.program
        { init = init start WindowResize
        , view = \{ model, size } -> lazy3 viewModelSizeToHtml view model size
        , update = simulateUpdate update
        , subscriptions = simulateSubs
        }


simulateUpdate update msg ({ model } as simulateModel) =
    case msg of
        TimeTick newTime ->
            let
                updatedModel =
                    update newTime model

                newModel =
                    -- necessary for lazy
                    if updatedModel == model then
                        model
                    else
                        updatedModel
            in
                { simulateModel | time = newTime, model = newModel } ! []

        WindowResize newSize ->
            { simulateModel | size = newSize } ! []

        _ ->
            simulateModel ! []


simulateSubs { time } =
    Sub.batch
        [ accumTimeSub time TimeTick
        , Window.resizes WindowResize
        ]


{-| -}
interact :
    model
    -> (model -> Collage.Form)
    -> (Msg -> model -> model)
    -> Interaction model
interact start view update =
    Html.program
        { init = init start WindowResize
        , view = \{ model, size } -> lazy3 viewModelSizeToHtml view model size
        , update = interactUpdate update
        , subscriptions = interactSubs
        }


interactUpdate update msg ({ model } as interactModel) =
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
        case msg of
            TimeTick newTime ->
                { interactModel | time = newTime, model = newModel } ! []

            WindowResize newSize ->
                { interactModel | size = newSize, model = newModel } ! []

            _ ->
                { interactModel | model = newModel } ! []


interactSubs { time } =
    Sub.batch
        [ accumTimeSub time TimeTick
        , Mouse.clicks MouseClick
        , Mouse.downs MouseDown
        , Mouse.ups MouseUp
        , Mouse.moves MouseMove
        , Keyboard.downs (toKey >> KeyDown)
        , Keyboard.ups (toKey >> KeyUp)
        , Window.resizes WindowResize
        ]


{-| Necessary for lazy! We need to define at the top level so that the
reference of the lazy fn is the same across calls.

From http://elm-lang.org/blog/blazing-fast-html:
So we just check to see if fn (viewModelSizeToHtml) and args (view and model) are
the same as last frame by comparing the old and new values by reference. This is
super cheap, and if they are the same, the lazy function can often avoid a ton
of work. This is a pretty simple trick that can speed things up significantly.
-}
viewModelSizeToHtml : (model -> Collage.Form) -> model -> Size -> Html.Html msg
viewModelSizeToHtml view model { width, height } =
    Collage.collage width height [ view model ] |> Element.toHtml


init : model -> (Size -> msg) -> ( Model model, Cmd msg )
init model tagger =
    Model { width = 0, height = 0 } 0 model ! [ Task.perform tagger Window.size ]


accumTimeSub : Time -> (Time -> msg) -> Sub msg
accumTimeSub time tagger =
    AnimationFrame.diffs ((+) time >> tagger)


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
