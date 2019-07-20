module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onFocus)



---- MODEL ----


type alias Model =
    { selectedMood : Mood
    , currentMood : Mood
    }


type alias Mood =
    { moodRating : Maybe MoodRating
    , moodComment : String
    }


type MoodRating
    = Happy
    | Neutral
    | Bad


init : ( Model, Cmd Msg )
init =
    let
        noMood =
            { moodRating = Nothing, moodComment = "" }
    in
    ( { selectedMood = noMood
      , currentMood = noMood
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SelectMood MoodRating


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectMood mood ->
            let
                oldCurrentMood =
                    model.currentMood

                newCurrentMood =
                    { oldCurrentMood
                        | moodRating = Just mood
                    }
            in
            ( { model | currentMood = newCurrentMood }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elm Niconico" ]
        , viewMoodSelector model.currentMood
        ]


viewMoodSelector : Mood -> Html Msg
viewMoodSelector mood =
    div []
        [ div []
            [ div [ onClick (SelectMood Happy) ] [ h1 [] [ text "😃" ] ]
            , h1 [ onClick (SelectMood Neutral) ] [ text "😐" ]
            , h1 [ onClick (SelectMood Bad) ] [ text "😐" ]
            ]
        , Html.hr [] []
        , div []
            [ h1 [] [ text ("Current Mood: " ++ showMood mood.moodRating) ] ]
        ]


showMood : Maybe MoodRating -> String
showMood maybeMoodRating =
    case maybeMoodRating of
        Just moodRating ->
            case moodRating of
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
