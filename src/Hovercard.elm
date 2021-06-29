module Hovercard exposing (Config, hovercard)

{-| This module makes rendering hovercards like [Wikipedia's](https://anandchowdhary.github.io/hovercard/) easy.

@docs Config, hovercard

-}

import Browser.Dom as Dom
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as HA
import Svg exposing (Svg)
import Svg.Attributes as SA


{-| Configure the hovercard.

  - maxWidth: maximum width of the hovercard
  - maxHeight: maximum height of the hovercard
  - borderColor, borderWidth, backgroundColor: minimal styling for the hovercard and the small arrow pointing to the element

-}
type alias Config =
    { maxWidth : Int
    , maxHeight : Int
    , borderColor : Color
    , backgroundColor : Color
    , borderWidth : Float
    }


{-| Render a hovercard above or below the given [Browser.Dom.Element](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#Element).

Call this function at the root of your HTML so the hovercard is positioned correctly.

Example:

    hovercard
        -- configuration
        { maxWidth = 100
        , maxHeight = 100
        , borderColor = Color.black
        , backgroundColor = Color.lightBlue
        , borderWidth = 2
        }
        -- Browser.Dom.Element representing
        -- viewport and position of the element
        element
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
hovercard : Config -> Dom.Element -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
hovercard { maxWidth, maxHeight, borderColor, backgroundColor, borderWidth } element attr hoverContent =
    let
        el =
            element.element

        vp =
            element.viewport

        -- position of el relative to vp
        x =
            el.x - vp.x

        y =
            el.y - vp.y

        diffBelow =
            vp.height
                - toFloat maxHeight
                - y
                - el.height

        diffAbove =
            y
                - toFloat maxHeight

        diffRight =
            vp.width - x

        diffLeft =
            x + el.width

        ( h, placeAbove ) =
            if diffAbove < 0 && diffBelow < 0 then
                if diffAbove < diffBelow then
                    ( toFloat maxHeight + diffBelow, False )

                else
                    ( toFloat maxHeight + diffAbove, True )

            else
                ( toFloat maxHeight, diffAbove >= 0 )

        ( w, placeLeft ) =
            if diffRight < toFloat maxWidth then
                ( min vp.width <| toFloat maxWidth
                , True
                )

            else
                ( toFloat maxWidth, False )

        offset =
            if placeLeft then
                -(vp.width - x - el.width)

            else if diffLeft < x then
                -x

            else if el.width > toFloat maxWidth then
                x + el.width / 2 - toFloat maxWidth / 2

            else
                0

        anchorH =
            if placeLeft then
                "right"

            else
                "left"

        anchorV =
            if placeAbove then
                "bottom"

            else
                "top"

        arrange =
            if placeAbove then
                identity

            else
                List.reverse

        triangleLength =
            16
    in
    Html.div
        [ HA.style "position" "absolute"
        , HA.style "top" <|
            (String.fromFloat <|
                if placeAbove then
                    el.y

                else
                    el.y + el.height
            )
                ++ "px"
        , HA.style "left" <| String.fromFloat el.x ++ "px"
        , HA.style "width" <| String.fromFloat el.width ++ "px"
        ]
        [ Html.div
            [ HA.style "position" "absolute"
            , HA.style "maxWidth" <| String.fromFloat w ++ "px"
            , HA.style "max-height" <| String.fromFloat h ++ "px"
            , HA.style anchorH "0"
            , HA.style "z-index" "100"
            , HA.style anchorV "100%"
            ]
            ([ Html.div
                ([ HA.style "overflow" "auto"
                 , HA.style "position" "relative"
                 , HA.style anchorV <| String.fromFloat (triangleLength / 2) ++ "px"
                 , HA.style "z-index" "1"
                 , HA.style anchorH <| String.fromFloat offset ++ "px"
                 , Color.toCssString backgroundColor
                    |> HA.style "background-color"
                 , Color.toCssString borderColor
                    |> HA.style "border-color"
                 , String.fromFloat borderWidth
                    ++ "px"
                    |> HA.style "border-width"
                 , HA.style "border-style" "solid"
                 ]
                    ++ attr
                )
                hoverContent
             , triangle
                { length = triangleLength
                , borderColor = borderColor
                , backgroundColor = backgroundColor
                , borderWidth = borderWidth
                , flip = placeAbove
                }
                [ HA.style "position" "absolute"
                , HA.style anchorH <| String.fromFloat (el.width / 2 - triangleLength / 2) ++ "px"
                , HA.style anchorV "0"
                , HA.style "z-index" "2"
                ]
             ]
                |> arrange
            )
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
