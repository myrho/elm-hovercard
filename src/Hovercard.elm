module Hovercard exposing
    ( Config
    , init
    , update
    , view
    , subscriptions
    , getElement
    , Model, Msg
    )

{-| This module makes rendering hovercards like [Wikipedia's](https://anandchowdhary.github.io/hovercard/) easy. It follows The Elm Architecture.


# Configuration

@docs Config


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


{-| Configure the hovercard.

  - tickLength: length of the tick
  - zIndex: z-index css property of the hovercard
  - borderColor, borderWidth, backgroundColor: minimal styling for the hovercard and the small arrow pointing to the element
  - viewport: override the viewport in which the hovercard can be positioned. This is needed if you render the hovercard within an HTML element with CSS attribute `position`.

-}
type alias Config =
    { tickLength : Float
    , zIndex : Int
    , borderColor : Color
    , backgroundColor : Color
    , borderWidth : Float
    , viewport : Maybe { x : Float, y : Float, width : Float, height : Float }
    }


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
hovercard { tickLength, borderColor, backgroundColor, borderWidth, viewport, zIndex } id target { size } attr hoverContent =
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
        [ HA.style "position" "absolute"
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
             , Color.toCssString backgroundColor
                |> HA.style "background-color"
             , Color.toCssString borderColor
                |> HA.style "border-color"
             , String.fromFloat borderWidth
                ++ "px"
                |> HA.style "border-width"
             , HA.style "border-style" "solid"
             , HA.id <| id ++ postfix
             ]
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


triangle : { length : Float, borderColor : Color, backgroundColor : Color, borderWidth : Float, flip : Bool } -> List (Svg.Attribute msg) -> Svg msg
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
            , Color.toCssString borderColor
                |> SA.stroke
            ]
            []
        , Svg.path
            [ SA.d path
            , Color.toCssString backgroundColor |> SA.fill
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
