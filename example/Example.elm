module Main exposing (main)

import Browser exposing (document)
import Browser.Dom as Dom
import Color
import Hovercard exposing (hovercard)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task
import Tuple exposing (..)


type Msg
    = HovercardMsg Hovercard.Msg
    | ClickElement String


main : Program () (Maybe Hovercard.Model) Msg
main =
    document
        { init = \_ -> ( Nothing, Cmd.none )
        , view =
            \model ->
                { title = "Hovercard"
                , body =
                    [ div
                        [ style "width" "100vw"
                        , style "height" "100vh"
                        , style "display" "flex"
                        , style "flex-direction" "column"
                        , style "justify-content" "space-between"
                        ]
                        [ div
                            [ style "width" "100%"
                            , style "display" "flex"
                            , style "justify-content" "space-between"
                            ]
                            [ div
                                [ style "width" "50px"
                                , style "height" "50px"
                                , style "background-color" "red"
                                , onClick (ClickElement "lefttop")
                                , id "lefttop"
                                ]
                                []
                            , div
                                [ style "width" "50px"
                                , style "height" "50px"
                                , style "background-color" "red"
                                , onClick (ClickElement "righttop")
                                , id "righttop"
                                ]
                                []
                            ]
                        , div
                            [ style "width" "100%"
                            , style "display" "flex"
                            , style "justify-content" "center"
                            ]
                            [ div
                                [ style "width" "50px"
                                , style "height" "50px"
                                , style "background-color" "red"
                                , onClick (ClickElement "middle")
                                , id "middle"
                                ]
                                []
                            ]
                        , div
                            [ style "width" "100%"
                            , style "display" "flex"
                            , style "justify-content" "center"
                            ]
                            [ div
                                [ style "width" "100%"
                                , style "height" "50px"
                                , style "background-color" "red"
                                , onClick (ClickElement "long")
                                , id "long"
                                ]
                                []
                            ]
                        , div
                            [ style "width" "100%"
                            , style "display" "flex"
                            , style "justify-content" "space-between"
                            ]
                            [ div
                                [ style "width" "50px"
                                , style "height" "50px"
                                , style "background-color" "red"
                                , onClick (ClickElement "leftbottom")
                                , id "leftbottom"
                                ]
                                []
                            , div
                                [ style "width" "50px"
                                , style "height" "50px"
                                , style "background-color" "red"
                                , onClick (ClickElement "rightbottom")
                                , id "rightbottom"
                                ]
                                []
                            ]
                        ]
                    ]
                        ++ (model
                                |> Maybe.map
                                    (\mod ->
                                        [ hovercard
                                            { tickLength = 16
                                            , zIndex = 1
                                            , borderColor = Color.black
                                            , backgroundColor = Color.lightBlue
                                            , borderWidth = 2
                                            , viewport = Nothing
                                            }
                                            mod
                                            [ style "box-shadow" "5px 5px 5px 0px rgba(0,0,0,0.25)"
                                            ]
                                            [ div
                                                [ style "white-space" "nowrap" ]
                                                [ text "Lorem ipsum dolor sit amet"
                                                ]
                                            ]
                                        ]
                                    )
                                |> Maybe.withDefault []
                           )
                }
        , update =
            \msg model ->
                case msg of
                    ClickElement id ->
                        let
                            ( hc, cmd ) =
                                Hovercard.init id
                        in
                        ( hc |> Just
                        , cmd
                            |> Cmd.map HovercardMsg
                        )

                    HovercardMsg hm ->
                        Maybe.map (Hovercard.update hm >> mapSecond (Cmd.map HovercardMsg)) model
                            |> Maybe.map (mapFirst Just)
                            |> Maybe.withDefault ( Nothing, Cmd.none )
        , subscriptions =
            Maybe.map Hovercard.subscriptions
                >> Maybe.map (Sub.map HovercardMsg)
                >> Maybe.withDefault Sub.none
        }
