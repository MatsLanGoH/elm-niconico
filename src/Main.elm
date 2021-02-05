module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (div, h1, text)
import Json.Decode as Decode exposing (Value)
import Page.Home as Home
import Page.Login as Login
import Route exposing (Route)
import Url exposing (Url)


type Model
    = Home
    | Login



-- MODEL


{-| TODO: Add Viewer to handle logged in user info
-}
init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    ( Home, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    let
        title =
            "Under construction"

        body =
            [ div []
                [ h1 [] [ text "Nothing to see here" ]
                ]
            ]
    in
    { title = title
    , body = body
    }



-- UPDATE


type Msg
    = NoOp
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page
            ( model, Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
