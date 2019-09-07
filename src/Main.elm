module Main exposing (main)

import Browser exposing (Document)
import Html exposing (div, h1, text)


type alias Model =
    { randString : String
    , randNum : Int
    }



-- MODEL


init : ( Model, Cmd Msg )
init =
    ( { randString = "Hello world"
      , randNum = 2
      }
    , Cmd.none
    )



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
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
