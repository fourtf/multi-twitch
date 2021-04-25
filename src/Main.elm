module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Html exposing (button, div, h1, h2, iframe, input, span, text)
import Html.Attributes exposing (placeholder, src, style)
import Html.Events exposing (onClick, onInput)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>))


type Msg
    = NopMsg
    | ChangeRoute Route
    | MsgUrlRequest UrlRequest
    | SetInputText String
    | OpenChannels


type Route
    = Home
    | Multi (List String)


type alias Model =
    { route : Route
    , key : Key
    , inputText : String
    }


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { route = Debug.log "route1" <| parseRoute url, key = key, inputText = "" }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title =
        case model.route of
            Home ->
                "Multi Twitch"

            Multi channels ->
                String.join ", " channels ++ " - Multi Twitch"
    , body = [ viewRoute model.route ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NopMsg ->
            ( model, Cmd.none )

        ChangeRoute r ->
            ( { model | route = Debug.log "route2" r }, Cmd.none )

        MsgUrlRequest (External url) ->
            ( model, Nav.load url )

        MsgUrlRequest (Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        SetInputText s ->
            ( { model | inputText = s }, Cmd.none )

        OpenChannels ->
            let
                channels =
                    model.inputText |> String.split "," |> List.map String.trim
            in
            ( model, Nav.pushUrl model.key <| Url.Builder.absolute channels [] )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlRequest : UrlRequest -> Msg
onUrlRequest =
    MsgUrlRequest


onUrlChange : Url -> Msg
onUrlChange =
    parseRoute >> ChangeRoute


parseRoute : Url -> Route
parseRoute url =
    case String.split "/" url.path |> List.filter (String.isEmpty >> not) of
        [] ->
            Home

        channels ->
            Multi channels


viewRoute : Route -> Html.Html Msg
viewRoute r =
    case r of
        Home ->
            div
                [ style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "width" "100vw"
                , style "height" "100vh"
                ]
                [ div []
                    [ h1
                        [ style "display" "block", style "text-align" "center" ]
                        [ text "Twitch Multichat " ]
                    , h2
                        [ style "display" "block", style "text-align" "center" ]
                        [ text "Displays different twitch chats in columns side by side. " ]
                    , div [ style "display" "flex", style "flex-direction" "row" ]
                        [ input
                            [ placeholder "channel 1, channel2, ..."
                            , onInput SetInputText
                            , style "flex" "1"
                            ]
                            []
                        , button [ onClick OpenChannels ] [ text "Open Channels" ]
                        ]
                    ]
                ]

        Multi channels ->
            div
                [ style "display" "flex"
                , style "flex-direction" "row"
                , style "width" "100vw"
                , style "height" "100vh"
                ]
                (List.map
                    (\channel ->
                        div
                            [ style "flex" "1"
                            , style "display" "flex"
                            , style "flex-direction" "column"
                            , style "border-right" "1px solid black"
                            ]
                            [ span
                                [ style "padding" "4px"
                                , style "background" "#333"
                                , style "color" "white"
                                , style "text-align" "center"
                                ]
                                [ text channel ]
                            , iframe
                                [ src <|
                                    "https://twitch.tv/embed/"
                                        ++ channel
                                        ++ "/chat?darkpopout=true&parent=multi.fourtf.com"
                                , style "flex" "1"
                                , style "border" "none"
                                ]
                                []
                            ]
                    )
                    channels
                )
