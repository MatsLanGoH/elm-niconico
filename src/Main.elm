module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, img, input, p, text)
import Html.Attributes exposing (disabled, src, value)
import Html.Events exposing (onClick, onFocus, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, decodeString, fail, field, float, int, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (fill, height, viewBox, width, x, y)
import Time exposing (Month, Weekday, toDay, toMonth, toWeekday, toYear, utc)
import Url.Builder as U exposing (crossOrigin)



---- TODO ----
-- [ ] Update newest mood
-- [ ] switch page view
---- MODEL ----


type alias Model =
    { currentMood : Mood
    , currentMoodRating : MoodRating
    , currentInput : String
    , currentTimeStamp : Time.Posix
    , moodList : MoodList
    , moodStatus : RequestMoodStatus
    , error : String
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


type alias MoodList =
    List Mood


type RequestMoodStatus
    = AwaitingMoodStatus
    | FailureMoodStatus
    | SuccessMoodStatus MoodList



---- DECODERS ----


moodListDecoder : Decoder MoodList
moodListDecoder =
    Decode.list moodDecoder


moodDecoder : Decoder Mood
moodDecoder =
    Decode.succeed Mood
        |> required "mood" moodRatingDecoder
        |> required "message" string
        |> required "timestamp" moodTimestampDecoder


moodRatingDecoder : Decoder MoodRating
moodRatingDecoder =
    Decode.int
        |> andThen
            (\n ->
                case n of
                    1 ->
                        succeed Happy

                    2 ->
                        succeed Neutral

                    3 ->
                        succeed Bad

                    _ ->
                        succeed Unset
            )


moodTimestampDecoder : Decoder Time.Posix
moodTimestampDecoder =
    Decode.map (\n -> n * 1000 |> round |> Time.millisToPosix)
        Decode.float


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
      , moodStatus = AwaitingMoodStatus
      , error = ""
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SelectMood MoodRating
    | UpdateCurrentInput String
    | SaveMood
    | FetchMoodList
    | GotMood (Result Http.Error String)
    | GotMoodList (Result Http.Error String)


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
            , let
                moodString =
                    "mood="
                        ++ (case newMood.moodRating of
                                Happy ->
                                    "1"

                                Neutral ->
                                    "2"

                                Bad ->
                                    "3"

                                Unset ->
                                    ""
                           )

                messageString =
                    "message=" ++ newMood.moodComment

                body =
                    Http.stringBody "application/x-www-form-urlencoded" (moodString ++ "&" ++ messageString)

                targetUrl =
                    crossOrigin "http://127.0.0.1:8000" [ "api/moods/" ] [ U.string "format" "json" ]
              in
              Http.post
                { body = body
                , url = targetUrl
                , expect = Http.expectString GotMood
                }
            )

        FetchMoodList ->
            ( { model | moodStatus = AwaitingMoodStatus }
            , let
                resultUrl =
                    crossOrigin "http://127.0.0.1:8000" [ "api/moods/" ] [ U.string "format" "json" ]
              in
              Http.get
                { url = resultUrl
                , expect = Http.expectString GotMoodList
                }
            )

        GotMood result ->
            case result of
                Ok fullJson ->
                    let
                        moodResponse =
                            decodeString moodDecoder fullJson
                    in
                    case moodResponse of
                        Ok data ->
                            ( { model | moodStatus = SuccessMoodStatus [ data ], currentMood = data }, Cmd.none )

                        Err error ->
                            ( { model | moodStatus = FailureMoodStatus, error = Decode.errorToString error }, Cmd.none )

                Err _ ->
                    ( { model | moodStatus = FailureMoodStatus }, Cmd.none )

        GotMoodList result ->
            case result of
                Ok fullJson ->
                    let
                        moodListResponse =
                            decodeString moodListDecoder fullJson
                    in
                    case moodListResponse of
                        Ok data ->
                            ( { model | moodStatus = SuccessMoodStatus data, moodList = data }, Cmd.none )

                        Err error ->
                            ( { model | moodStatus = FailureMoodStatus, error = Decode.errorToString error }, Cmd.none )

                Err _ ->
                    ( { model | moodStatus = FailureMoodStatus }, Cmd.none )



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
        , div []
            [ button
                [ onClick FetchMoodList ]
                [ text "Fetch Moods" ]
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
