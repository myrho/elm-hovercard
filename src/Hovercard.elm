module Hovercard exposing
    ( Config, defaultConfig, withBackgroundColor, withBackgroundColorString, withBorderColor, withBorderColorString, withBorderWidth, withFixed, withTickLength, styleTickOnly, withViewport, withZIndex
    , init
    , update
    , view
    , subscriptions
    , getElement
    , Model, Msg
    )

{-| This module makes rendering hovercards like [Wikipedia's](https://anandchowdhary.github.io/hovercard/) easy. It follows The Elm Architecture.


# Configuration

@docs Config, defaultConfig, withBackgroundColor, withBackgroundColorString, withBorderColor, withBorderColorString, withBorderWidth, withFixed, withTickLength, styleTickOnly, withViewport, withZIndex


# Init

@docs init


# Update

@docs update


# View

@docs view


# Subscriptions

@docs subscriptions


# Commands

@docs getElement


# Internals

@docs Model, Msg

-}

import Browser.Dom as Dom
import Browser.Events
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as HA
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task
import Tuple exposing (first, second)


{-| The configuration of the Hovercard
-}
type Config
    = Config ConfigInternal


{-| Internal representation of border color - can be a Color or a CSS string
-}
type BorderColor
    = BorderColorType Color
    | BorderColorString String


{-| Internal representation of background color - can be a Color or a CSS string
-}
type BackgroundColor
    = BackgroundColorType Color
    | BackgroundColorString String


{-| Configure the hovercard.

  - tickLength: length of the tick
  - zIndex: z-index css property of the hovercard
  - borderColor, borderWidth, backgroundColor: minimal styling for the hovercard and the small arrow pointing to the element
  - viewport: override the viewport in which the hovercard can be positioned. This is needed if you render the hovercard within an HTML element with CSS attribute `position`.
  - fixed: use `position: fixed` for rendering the hovercard.

-}
type alias ConfigInternal =
    { tickLength : Float
    , zIndex : Int
    , borderColor : BorderColor
    , backgroundColor : BackgroundColor
    , borderWidth : Float
    , viewport : Maybe { x : Float, y : Float, width : Float, height : Float }
    , fixed : Bool
    , tickOnlyStyle : Bool
    }


{-| Default configuration for the hovercard.

  - tickLength: 16
  - zIndex: 9999
  - borderColor: Color.gray
  - backgroundColor: Color.white
  - borderWidth: 1
  - viewport: Nothing
  - fixed: False
  - tickOnlyStyle: False

-}
defaultConfig : Config
defaultConfig =
    Config
        { tickLength = 16
        , zIndex = 9999
        , borderColor = BorderColorType Color.gray
        , backgroundColor = BackgroundColorType Color.white
        , borderWidth = 1
        , viewport = Nothing
        , fixed = False
        , tickOnlyStyle = False
        }


{-| Create a configuration with a custom tick length.
-}
withTickLength : Float -> Config -> Config
withTickLength tickLength (Config config) =
    Config { config | tickLength = tickLength }


{-| Set the z-index of the hovercard.
-}
withZIndex : Int -> Config -> Config
withZIndex zIndex (Config config) =
    Config { config | zIndex = zIndex }


{-| Set the border color of the hovercard.
-}
withBorderColor : Color -> Config -> Config
withBorderColor borderColor (Config config) =
    Config { config | borderColor = BorderColorType borderColor }


{-| Set the border color of the hovercard as a CSS string.
-}
withBorderColorString : String -> Config -> Config
withBorderColorString borderColor (Config config) =
    Config { config | borderColor = BorderColorString borderColor }


{-| Set the background color of the hovercard.
-}
withBackgroundColor : Color -> Config -> Config
withBackgroundColor backgroundColor (Config config) =
    Config { config | backgroundColor = BackgroundColorType backgroundColor }


{-| Set the background color of the hovercard as a CSS string.
-}
withBackgroundColorString : String -> Config -> Config
withBackgroundColorString backgroundColor (Config config) =
    Config { config | backgroundColor = BackgroundColorString backgroundColor }


{-| Set the border width of the hovercard.
-}
withBorderWidth : Float -> Config -> Config
withBorderWidth borderWidth (Config config) =
    Config { config | borderWidth = borderWidth }


{-| Set the viewport for the hovercard positioning.
-}
withViewport : Maybe { x : Float, y : Float, width : Float, height : Float } -> Config -> Config
withViewport viewport (Config config) =
    Config { config | viewport = viewport }


{-| Set whether the hovercard uses fixed positioning.
-}
withFixed : Bool -> Config -> Config
withFixed fixed (Config config) =
    Config { config | fixed = fixed }


{-| Set whether only the triangle gets background and border applied.
-}
styleTickOnly : Bool -> Config -> Config
styleTickOnly flag (Config config) =
    Config { config | tickOnlyStyle = flag }


{-| Hovercards internal `Msg`s
-}
type Msg
    = GotTargetElement (Result Dom.Error Dom.Element)
    | GotHovercardElement (Result Dom.Error Dom.Element)
    | WindowResized


{-| Hovercards internal model
-}
type Model
    = Model ModelInternal


type alias ModelInternal =
    { id : String
    , target : Maybe Dom.Element
    , size : Maybe ( Float, Float )
    }


{-| Initialize the hovercard with the ID of the target HTML element.

It tries to find the target element in the DOM using [Browser.Dom.getElement](https://package.elm-lang.org/packages/elm/browser/1.0.2/Browser-Dom#getElement).

-}
init : String -> ( Model, Cmd Msg )
init id =
    let
        model =
            { id = id
            , target = Nothing
            , size = Nothing
            }
                |> Model
    in
    ( model
    , getElement model
    )


{-| Trigger positioning of the hovercard to the target element programmatically.
-}
getElement : Model -> Cmd Msg
getElement (Model { id }) =
    Dom.getElement id
        |> Task.attempt GotTargetElement


postfix : String
postfix =
    "_hc"


{-| Convert BorderColor to a CSS string value
-}
borderColorToCssString : BorderColor -> String
borderColorToCssString borderColor =
    case borderColor of
        BorderColorType color ->
            Color.toCssString color

        BorderColorString str ->
            str


{-| Convert BackgroundColor to a CSS string value
-}
backgroundColorToCssString : BackgroundColor -> String
backgroundColorToCssString backgroundColor =
    case backgroundColor of
        BackgroundColorType color ->
            Color.toCssString color

        BackgroundColorString str ->
            str


{-| Update the hovercard model
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        GotTargetElement result ->
            result
                |> Result.map
                    (\element ->
                        ( Model
                            { model
                                | target = Just element
                            }
                        , model.id
                            ++ postfix
                            |> Dom.getElement
                            |> Task.attempt GotHovercardElement
                        )
                    )
                |> Result.withDefault ( Model model, Cmd.none )

        GotHovercardElement result ->
            ( Model
                { model
                    | size =
                        result
                            |> Result.toMaybe
                            |> Maybe.map (\{ element } -> ( element.width, element.height ))
                }
            , Cmd.none
            )

        WindowResized ->
            ( Model model
            , getElement (Model model)
            )


{-| Render the hovercard.

Example:

    view
        -- configuration
        { tickLength = 16
        , zIndex = 1
        , borderColor = Color.black
        , backgroundColor = Color.lightBlue
        , borderWidth = 2
        , viewport = Nothing
        }
        -- Hovercard model
        model
        -- additional styles for the hovercard, eg. a shadow
        [ style "box-shadow" "5px 5px 5px 0px rgba(0,0,0,0.25)"
        ]
        -- the content of the hovercard
        [ div
            []
            [ text "Lorem ipsum dolor sit amet"
            ]
        ]

-}
view : Config -> Model -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
view config (Model model) attr content =
    Maybe.map (\target -> hovercard config model.id target model attr content) model.target
        |> Maybe.withDefault (Html.span [] [])


hovercard : Config -> String -> Dom.Element -> ModelInternal -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
hovercard (Config { tickLength, borderColor, backgroundColor, borderWidth, viewport, zIndex, fixed, tickOnlyStyle }) id target { size } attr hoverContent =
    let
        el =
            target.element

        vp =
            viewport
                |> Maybe.withDefault target.viewport

        width =
            Maybe.map first size
                |> Maybe.withDefault 0

        height =
            Maybe.map second size
                |> Maybe.withDefault 0

        leftElBound =
            max el.x vp.x

        rightElBound =
            min (el.x + el.width) (vp.x + vp.width)

        upperElBound =
            max el.y vp.y

        lowerElBound =
            min (el.y + el.height) (vp.y + vp.height)

        customVpX =
            Maybe.map .x viewport |> Maybe.withDefault 0

        anchorX =
            if size == Nothing then
                0

            else
                leftElBound
                    + (rightElBound - leftElBound)
                    / 2
                    - customVpX

        diffAbove =
            upperElBound - vp.y

        diffBelow =
            vp.y + vp.height - lowerElBound

        placeBelow =
            diffAbove
                < height
                + tickLength

        customVpY =
            Maybe.map .y viewport |> Maybe.withDefault 0

        anchorY =
            if size == Nothing then
                0

            else if placeBelow then
                lowerElBound - customVpY

            else
                upperElBound - customVpY

        rightVpBound =
            vp.x + vp.width

        halfWidth =
            width / 2

        rightCardBound =
            anchorX + halfWidth

        leftCardBound =
            anchorX - halfWidth

        diffLeft =
            min 0 leftCardBound

        diffRight =
            rightVpBound
                - rightCardBound
                - customVpX
                |> min 0

        cardX =
            -halfWidth
                - diffLeft
                + diffRight

        --  |> max 0
        cardY =
            if placeBelow then
                tickLength / 2

            else
                -tickLength / 2 - height
    in
    Html.div
        [ HA.style "position" <|
            if fixed then
                "fixed"

            else
                "absolute"
        , HA.style "top" <| String.fromFloat anchorY ++ "px"
        , HA.style "left" <| String.fromFloat anchorX ++ "px"
        , HA.style "width" "1px"
        , HA.style "visibility" <|
            if size == Nothing then
                "hidden"

            else
                "visible"
        , HA.style "z-index" <| String.fromInt zIndex
        ]
        [ Html.div
            ([ HA.style "position" "absolute"
             , HA.style "left" <| String.fromFloat cardX ++ "px"
             , HA.style "top" <| String.fromFloat cardY ++ "px"
             ]
                ++ (if tickOnlyStyle then
                    []

                   else
                    [ backgroundColorToCssString backgroundColor
                        |> HA.style "background-color"
                    , borderColorToCssString borderColor
                        |> HA.style "border-color"
                    , String.fromFloat borderWidth
                        ++ "px"
                        |> HA.style "border-width"
                    , HA.style "border-style" "solid"
                    ]
                   )
                ++ [ HA.id <| id ++ postfix ]
                ++ attr
            )
            hoverContent
        , triangle
            { length = tickLength
            , borderColor = borderColor
            , backgroundColor = backgroundColor
            , borderWidth = borderWidth
            , flip = not placeBelow
            }
            [ HA.style "position" "absolute"
            , HA.style
                (if placeBelow then
                    "top"

                 else
                    "bottom"
                )
                "0px"
            , HA.style "left" <| String.fromFloat (-(tickLength / 2) + borderWidth / 2) ++ "px"
            , HA.style "z-index" "2"
            ]
        ]


triangle : { length : Float, borderColor : BorderColor, backgroundColor : BackgroundColor, borderWidth : Float, flip : Bool } -> List (Svg.Attribute msg) -> Svg msg
triangle { length, borderColor, backgroundColor, borderWidth, flip } attr =
    let
        tl =
            String.fromFloat length

        tl2 =
            String.fromFloat <| length / 2

        tl3 =
            String.fromFloat <| length / 2 + borderWidth

        path =
            if flip then
                "M 0,0 " ++ tl2 ++ "," ++ tl2 ++ " " ++ tl ++ ",0"

            else
                "M 0," ++ tl3 ++ " " ++ tl2 ++ ",1" ++ " " ++ tl ++ "," ++ tl3
    in
    Svg.svg
        ([ SA.width tl
         , SA.height tl3
         ]
            ++ attr
        )
        [ Svg.path
            [ SA.d path
            , SA.strokeLinecap "round"
            , borderWidth * 1.5 |> String.fromFloat |> SA.strokeWidth
            , borderColorToCssString borderColor
                |> SA.stroke
            ]
            []
        , Svg.path
            [ SA.d path
            , backgroundColorToCssString backgroundColor |> SA.fill
            ]
            []
        ]


{-| Subscribes to [Browser.Dom.onResize](https://package.elm-lang.org/packages/elm/browser/1.0.2/Browser-Events#onResize) in order to reposition the hovercard automatically.
-}
subscriptions : Model -> Sub Msg
subscriptions (Model { size }) =
    size
        |> Maybe.map
            (\_ ->
                Browser.Events.onResize (\_ _ -> WindowResized)
            )
        |> Maybe.withDefault Sub.none
