module Collage.Program
    exposing
        ( DrawingProgram
        , drawingProgram
        , AnimationProgram
        , animationProgram
        , SimulationProgram
        , simulationProgram
        , InteractiveProgram
        , interactiveProgram
        , Msg(..)
        , Mouse
        , Key(..)
        , Window
        )

{-| Incremental introduction to interactive graphics programs.

# Draw
@docs DrawingProgram, drawingProgram

# Draw with Time
@docs AnimationProgram, animationProgram

# Draw with Time and Model
@docs SimulationProgram, simulationProgram

# Draw with Model and Messages
@docs InteractiveProgram, interactiveProgram, Msg, Mouse, Key, Window
-}

import AnimationFrame
import Element
import Collage
import Html
import Html.Lazy exposing (lazy3)
import Keyboard exposing (KeyCode)
import Mouse exposing (Position)
import Task
import Time exposing (Time)
import Window exposing (Size)


{-| -}
type alias Mouse =
    { x : Float, y : Float }


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
type alias Window =
    { top : Float
    , bottom : Float
    , left : Float
    , right : Float
    }


{-| -}
type Msg
    = TimeTick Time
    | MouseClick Mouse
    | MouseDown Mouse
    | MouseUp Mouse
    | MouseMove Mouse
    | KeyDown Key
    | KeyUp Key
    | WindowResize Window


type alias Model model =
    { window : Window
    , time : Time
    , model : model
    }


{-| -}
type alias DrawingProgram =
    Program Never (Model ()) Msg


{-| -}
type alias AnimationProgram =
    Program Never (Model ()) Msg


{-| -}
type alias SimulationProgram model =
    Program Never (Model model) Msg


{-| -}
type alias InteractiveProgram model =
    Program Never (Model model) Msg


{-| -}
drawingProgram : Collage.Form -> DrawingProgram
drawingProgram view =
    Html.program
        { init = wrapStudentInit ()
        , view = \{ window } -> viewModelWindowToHtml (\_ -> view) () window
        , update = drawUpdate
        , subscriptions = \_ -> windowResizeSub
        }


drawUpdate : Msg -> Model () -> ( Model (), Cmd Msg )
drawUpdate msg model =
    case msg of
        WindowResize newWindow ->
            { model | window = newWindow } ! []

        _ ->
            model ! []


{-| -}
animationProgram : (Time -> Collage.Form) -> AnimationProgram
animationProgram view =
    Html.program
        { init = wrapStudentInit ()
        , view = \{ time, window } -> viewModelWindowToHtml view time window
        , update = animateUpdate
        , subscriptions = animateSubs
        }


animateUpdate : Msg -> Model () -> ( Model (), Cmd Msg )
animateUpdate msg model =
    case msg of
        TimeTick newTime ->
            { model | time = newTime } ! []

        WindowResize newWindow ->
            { model | window = newWindow } ! []

        _ ->
            model ! []


animateSubs : Model () -> Sub Msg
animateSubs { time } =
    Sub.batch
        [ accumTimeSub time
        , windowResizeSub
        ]


{-| -}
simulationProgram :
    { init : model
    , view : model -> Collage.Form
    , update : Time -> model -> model
    }
    -> SimulationProgram model
simulationProgram { init, view, update } =
    Html.program
        { init = wrapStudentInit init
        , view = \{ model, window } -> lazy3 viewModelWindowToHtml view model window
        , update = simulateUpdate update
        , subscriptions = simulateSubs
        }


simulateUpdate : (Time -> model -> model) -> Msg -> Model model -> ( Model model, Cmd Msg )
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

        WindowResize newWindow ->
            { simulateModel | window = newWindow } ! []

        _ ->
            simulateModel ! []


simulateSubs : Model model -> Sub Msg
simulateSubs { time } =
    Sub.batch
        [ accumTimeSub time
        , windowResizeSub
        ]


{-| -}
interactiveProgram :
    { init : model
    , view : model -> Collage.Form
    , update : Msg -> model -> model
    }
    -> InteractiveProgram model
interactiveProgram { init, view, update } =
    Html.program
        { init = wrapStudentInit init
        , view = \{ model, window } -> lazy3 viewModelWindowToHtml view model window
        , update = interactUpdate update
        , subscriptions = interactSubs
        }


interactUpdate : (Msg -> model -> model) -> Msg -> Model model -> ( Model model, Cmd Msg )
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

            WindowResize newWindow ->
                { interactModel | window = newWindow, model = newModel } ! []

            _ ->
                { interactModel | model = newModel } ! []


interactSubs : Model model -> Sub Msg
interactSubs { time, window } =
    Sub.batch
        [ accumTimeSub time
        , Mouse.clicks ((mouseToCartCoord window) >> MouseClick)
        , Mouse.downs ((mouseToCartCoord window) >> MouseDown)
        , Mouse.ups ((mouseToCartCoord window) >> MouseUp)
        , Mouse.moves ((mouseToCartCoord window) >> MouseMove)
        , Keyboard.downs (toKey >> KeyDown)
        , Keyboard.ups (toKey >> KeyUp)
        , windowResizeSub
        ]


{-| Necessary for lazy! We need to define at the top level so that the
reference of the lazy fn is the same across calls.

From http://elm-lang.org/blog/blazing-fast-html:
So we just check to see if fn (viewModelWindowToHtml) and args (view, model, and window) are
the same as last frame by comparing the old and new values by reference. This is
super cheap, and if they are the same, the lazy function can often avoid a ton
of work. This is a pretty simple trick that can speed things up significantly.
-}
viewModelWindowToHtml : (model -> Collage.Form) -> model -> Window -> Html.Html msg
viewModelWindowToHtml view model window =
    let
        { width, height } =
            windowToSize window
    in
        Collage.collage width height [ view model ]
            |> Element.toHtml


wrapStudentInit : model -> ( Model model, Cmd Msg )
wrapStudentInit model =
    let
        windowCmd =
            Task.perform (sizeToWindow >> WindowResize) Window.size
    in
        Model (Window 0 0 0 0) 0 model ! [ windowCmd ]


windowResizeSub : Sub Msg
windowResizeSub =
    Window.resizes (sizeToWindow >> WindowResize)


accumTimeSub : Time -> Sub Msg
accumTimeSub time =
    AnimationFrame.diffs ((\diff -> diff + time) >> TimeTick)


mouseToCartCoord : Window -> Position -> Mouse
mouseToCartCoord { right, top } { x, y } =
    { x = toFloat x - right, y = top - toFloat y }


sizeToWindow : Size -> Window
sizeToWindow { width, height } =
    let
        w =
            toFloat width / 2

        h =
            toFloat height / 2
    in
        { top = h
        , bottom = -h
        , left = -w
        , right = w
        }


windowToSize : Window -> Size
windowToSize { top, bottom, left, right } =
    let
        width =
            round (right - left)

        height =
            round (top - bottom)
    in
        { width = width, height = height }


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
