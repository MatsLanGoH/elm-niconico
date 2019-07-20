module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onFocus)



---- MODEL ----


type alias Model =
    { selectedMood : Maybe Mood
    , currentMood : Maybe Mood
    }


type Mood
    = Happy
    | Neutral
    | Bad


init : ( Model, Cmd Msg )
init =
    ( { selectedMood = Nothing
      , currentMood = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SelectMood Mood


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectMood mood ->
            ( { model | currentMood = Just mood }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elm Niconico" ]
        , div []
            [ div [ onClick (SelectMood Happy) ] [ h1 [] [ text "😃" ] ]
            , h1 [ onClick (SelectMood Neutral) ] [ text "😐" ]
            , h1 [ onClick (SelectMood Bad) ] [ text "😐" ]
            ]
        , Html.hr [] []
        , div []
            [ h1 [] [ text ("Current Mood: " ++ showMood model.currentMood) ] ]
        ]


showMood : Maybe Mood -> String
showMood maybeMood =
    case maybeMood of
        Just mood ->
            case mood of
                Happy ->
                    "Happy"

                Neutral ->
                    "Neutral"

                Bad ->
                    "Bad"

        Nothing ->
            "Undecided."



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
