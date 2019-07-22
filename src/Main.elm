module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, img, input, p, text)
import Html.Attributes exposing (disabled, src, value)
import Html.Events exposing (onClick, onFocus, onInput)
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (height, viewBox, width, x, y, fill)
import Time exposing (Month, Weekday, toDay, toMonth, toWeekday, toYear, utc)



---- TODO ----
-- add dashboard
--  o text only first
--  o then use svg-rendered icons
--  - fix spacings
---- MODEL ----


type alias Model =
    { currentMood : Mood
    , currentMoodRating : MoodRating
    , currentInput : String
    , currentTimeStamp : Time.Posix
    , moodList : List Mood
    }


type alias Mood =
    { moodRating : MoodRating
    , moodComment : String
    , moodTimeStamp : Time.Posix
    }


type MoodRating
    = Happy
    | Neutral
    | Bad
    | Unset


init : ( Model, Cmd Msg )
init =
    let
        noMood =
            { moodRating = Unset, moodComment = "", moodTimeStamp = Time.millisToPosix 0 }
    in
    ( { currentMood = noMood
      , currentMoodRating = Unset
      , currentInput = ""
      , currentTimeStamp = Time.millisToPosix 0
      , moodList = []
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
            ( { model | currentMoodRating = moodRating }, Cmd.none )

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
                , currentMoodRating = Unset
                , currentInput = ""
                , moodList = List.append model.moodList [ newMood ] -- No other way than List.append?
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elm Niconico" ]
        , viewMoodSelector model
        , Html.hr [] []
        , viewMoodDetails model.currentMood
        , Html.hr [] []
        , viewMoodDashboard model
        ]


viewMoodSelector : Model -> Html Msg
viewMoodSelector model =
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
        ]


viewMoodDetails : Mood -> Html Msg
viewMoodDetails mood =
    div []
        [ h1 [] [ text "Current Mood: " ]
        , p [] [ text (showMood mood.moodRating) ]
        , p [] [ text mood.moodComment ]
        ]


viewMoodDashboard : Model -> Html Msg
viewMoodDashboard model =
    div []
        [ h1 [] [ text "Mood Dashboard: " ]
        , div []
            [ text "Mood Count: "
            , text <| String.fromInt <| List.length model.moodList
            ]
        , div [] (viewMoodIcons model.moodList)
        ]


viewMoodIcons : List Mood -> List (Html Msg)
viewMoodIcons moodList =
    let
        moodColor mood =
            case mood.moodRating of
                Happy ->
                    "rgb(82,255,165)"

                Neutral ->
                    "rgb(232,225,92)"

                Bad ->
                    "rgb(235,96,136)"

                Unset ->
                    "rgb(204,204,204)"
        block mood =
            svg
                [ width "24"
                , height "24"
                , viewBox "0 0 24 24"
                ]
                [ rect
                    [ x "2"
                    , y "2"
                    , width "18"
                    , height "18"
                    , fill (moodColor mood)
                    ]
                    []
                ]
    in
    List.map (\m -> block m) moodList


showMood : MoodRating -> String
showMood moodRating =
    case moodRating of
        Happy ->
            "Happy"

        Neutral ->
            "Neutral"

        Bad ->
            "Bad"

        Unset ->
            "Undecided."


hasMood : MoodRating -> Bool
hasMood moodRating =
    case moodRating of
        Unset ->
            False

        _ ->
            True



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
