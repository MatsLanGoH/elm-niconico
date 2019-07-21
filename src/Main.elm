module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, img, input, text, p)
import Html.Attributes exposing (src, value )
import Html.Events exposing (onClick, onFocus, onInput)

---- TODO ----

-- selectMood should only select a MoodRating (only keep for selection)
-- submitButton should save MoodRating and Message and Timestamp to a Mood


---- MODEL ----


type alias Model =
    { selectedMood : Mood
    , currentMood : Mood
    , currentInput : String
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
      , currentInput = ""
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SelectMood MoodRating
    | UpdateCurrentInput String


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
        UpdateCurrentInput input ->
            ( { model | currentInput = input}, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elm Niconico" ]
        , viewMoodSelector model model.currentMood
        ]


viewMoodSelector : Model -> Mood -> Html Msg
viewMoodSelector model mood =
    div []
        [ div []
            [ h1 [ onClick (SelectMood Happy) ] [ text "😃" ] 
            , h1 [ onClick (SelectMood Neutral) ] [ text "😐" ]
            , h1 [ onClick (SelectMood Bad) ] [ text "😐" ]
            ]
        , div []
            [ div []
                [ input [ value model.currentInput, onInput UpdateCurrentInput ] [] ]
            ]
        , div []
            [ button [ ] [text "Submit"] ]
        , Html.hr [] []
        , div []
            [ h1 [] [ text ("Current Mood: ")]
            , p [] [ text (showMood mood.moodRating)]
            , p [] [ text model.currentInput ] ]
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
