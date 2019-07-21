module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, img, input, p, text)
import Html.Attributes exposing (disabled, src, value)
import Html.Events exposing (onClick, onFocus, onInput)
import Time exposing (Month, Weekday, toDay, toMonth, toWeekday, toYear, utc)



---- TODO ----
-- submitButton should save MoodRating and Message and Timestamp to a Mood
---- MODEL ----


type alias Model =
    { currentMood : Mood
    , currentMoodRating : Maybe MoodRating
    , currentInput : String
    , currentTimeStamp : Time.Posix
    }


type alias Mood =
    { moodRating : Maybe MoodRating
    , moodComment : String
    , moodTimeStamp : Time.Posix
    }


type MoodRating
    = Happy
    | Neutral
    | Bad


init : ( Model, Cmd Msg )
init =
    let
        noMood =
            { moodRating = Nothing, moodComment = "", moodTimeStamp = Time.millisToPosix 0 }
    in
    ( { currentMood = noMood
      , currentMoodRating = Nothing
      , currentInput = ""
      , currentTimeStamp = Time.millisToPosix 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SelectMood MoodRating
    | UpdateCurrentInput String
    | SaveMood


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectMood moodRating ->
            ( { model | currentMoodRating = Just moodRating }, Cmd.none )

        UpdateCurrentInput input ->
            ( { model | currentInput = input }, Cmd.none )

        SaveMood ->
            let
                currentMood =
                    model.currentMood

                newMood =
                    { currentMood
                        | moodRating = model.currentMoodRating
                        , moodComment = model.currentInput
                    }
            in
            ( { model
                | currentMood = newMood
                , currentMoodRating = Nothing
                , currentInput = ""
              }
            , Cmd.none
            )



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
            [ h1 [ onClick (SelectMood Happy) ] [ text ":)" ]
            , h1 [ onClick (SelectMood Neutral) ] [ text ":|" ]
            , h1 [ onClick (SelectMood Bad) ] [ text ":(" ]
            ]
        , div []
            [ div []
                [ p [] [ text "How do you feel?" ]
                , input [ value model.currentInput, onInput UpdateCurrentInput ] []
                ]
            ]
        , div []
            [ button
                [ disabled (not <| hasMood model.currentMoodRating)
                , onClick SaveMood
                ]
                [ text "Submit" ]
            ]
        , Html.hr [] []
        , div []
            [ h1 [] [ text "Current Mood: " ]
            , p [] [ text (showMood mood.moodRating) ]
            , p [] [ text mood.moodComment ]
            ]
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


hasMood : Maybe MoodRating -> Bool
hasMood maybeMoodRating =
    case maybeMoodRating of
        Just _ ->
            True

        Nothing ->
            False



---- UTILS ----


toUtcString : Time.Posix -> String
toUtcString date =
    String.fromInt (Time.toYear utc date)
        ++ "/"
        ++ String.fromInt (Time.toHour utc date)
        ++ ":"
        ++ String.fromInt (Time.toMinute utc date)
        ++ ":"
        ++ String.fromInt (Time.toSecond utc date)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
