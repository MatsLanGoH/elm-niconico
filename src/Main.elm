module Main exposing (main)

{-| ---- TODO: Auth ----
[ ] get user info from api/auth/user
[ ] Implement register form
[ ] (or redirect to login)
[ ] redirect to overview
-}

import Browser exposing (Document)
import Html exposing (Html, a, button, div, footer, h1, i, input, li, nav, p, text, ul)
import Html.Attributes exposing (class, classList, disabled, href, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Lazy exposing (lazy)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, decodeString, float, int, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Svg exposing (rect, svg, title)
import Svg.Attributes exposing (fill, height, viewBox, width, x, y)
import Time exposing (toYear, utc)
import Url.Builder as U exposing (crossOrigin)


{-| --- TODO: Pages

  - [x] implement multiple pages
  - [ ] select login view when no token

-}



---- MODEL ----


type alias Model =
    { page : Page
    , form : Form
    , token : Maybe KnoxToken
    , loginStatus : LoginStatus
    , currentMood : Mood
    , currentMoodRating : MoodRating
    , currentInput : String
    , currentTimeStamp : Time.Posix
    , moodList : MoodList
    , moodStatus : RequestMoodStatus
    , error : String
    }


type Page
    = Login
    | Moods


type alias Form =
    { username : String
    , password : String
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


type alias KnoxToken =
    { expiry : String
    , token : String
    }


type LoginStatus
    = LoggedIn
    | LoggedOut


type RequestMoodStatus
    = AwaitingMoodStatus
    | FailureMoodStatus
    | SuccessMoodStatus MoodList



---- DECODERS ----


knoxTokenDecoder : Decoder KnoxToken
knoxTokenDecoder =
    Decode.succeed KnoxToken
        |> required "expiry" string
        |> required "token" string


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
    ( { page = Login
      , form =
            { username = ""
            , password = ""
            }
      , token = Nothing
      , loginStatus = LoggedOut
      , currentMood = noMood
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
    = ChangedPage Page
    | SubmittedForm
    | EnterUsername String
    | EnterPassword String
    | SelectMood MoodRating
    | UpdateCurrentInput String
    | SaveMood
    | FetchMoodList
    | GotAuthToken (Result Http.Error String)
    | GotMood (Result Http.Error String)
    | GotMoodList (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedPage page ->
            ( { model | page = page }, Cmd.none )

        SubmittedForm ->
            ( model
            , let
                toJson =
                    login model.form

                resultUrl =
                    crossOrigin "http://127.0.0.1:8000" [ "api/auth/login/" ] []
              in
              Http.request
                { method = "POST"
                , url = resultUrl
                , headers = []
                , body = Http.jsonBody toJson
                , timeout = Nothing
                , tracker = Nothing
                , expect = Http.expectString GotAuthToken
                }
            )

        EnterUsername username ->
            updateForm (\form -> { form | username = username }) model

        EnterPassword password ->
            updateForm (\form -> { form | password = password }) model

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

                token =
                    case model.token of
                        Just t ->
                            t.token

                        Nothing ->
                            ""
              in
              Http.request
                { method = "POST"
                , url = targetUrl
                , headers = [ Http.header "Authorization" ("Token " ++ token) ]
                , body = body
                , timeout = Nothing
                , tracker = Nothing
                , expect = Http.expectString GotMood
                }
            )

        FetchMoodList ->
            ( { model | moodStatus = AwaitingMoodStatus }
            , let
                resultUrl =
                    crossOrigin "http://127.0.0.1:8000" [ "api/moods/" ] [ U.string "format" "json" ]

                token =
                    case model.token of
                        Just t ->
                            t.token

                        Nothing ->
                            ""
              in
              Http.request
                { method = "GET"
                , url = resultUrl
                , headers = [ Http.header "Authorization" ("Token " ++ token) ]
                , body = Http.emptyBody
                , timeout = Nothing
                , tracker = Nothing
                , expect = Http.expectString GotMoodList
                }
            )

        GotAuthToken result ->
            case result of
                Ok fullJson ->
                    let
                        authTokenResponse =
                            decodeString knoxTokenDecoder fullJson
                    in
                    case authTokenResponse of
                        -- Todo: expiry
                        Ok data ->
                            ( { model
                                | loginStatus = LoggedIn
                                , token = Just data
                              }
                            , Cmd.none
                            )

                        Err error ->
                            ( { model
                                | loginStatus = LoggedOut
                                , token = Nothing
                                , error = Decode.errorToString error
                              }
                            , Cmd.none
                            )

                Err _ ->
                    ( { model
                        | loginStatus = LoggedOut
                        , token = Nothing
                      }
                    , Cmd.none
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
                            ( { model
                                | moodStatus = SuccessMoodStatus [ data ]
                                , currentMood = data
                              }
                            , Cmd.none
                            )

                        Err error ->
                            ( { model
                                | moodStatus = FailureMoodStatus
                                , error = Decode.errorToString error
                              }
                            , Cmd.none
                            )

                Err _ ->
                    ( { model
                        | moodStatus = FailureMoodStatus
                      }
                    , Cmd.none
                    )

        GotMoodList result ->
            case result of
                Ok fullJson ->
                    let
                        moodListResponse =
                            decodeString moodListDecoder fullJson
                    in
                    case moodListResponse of
                        Ok data ->
                            ( { model
                                | moodStatus = SuccessMoodStatus data
                                , moodList = data
                              }
                            , Cmd.none
                            )

                        Err error ->
                            ( { model
                                | moodStatus = FailureMoodStatus
                                , moodList = []
                                , error = Decode.errorToString error
                              }
                            , Cmd.none
                            )

                Err _ ->
                    ( { model
                        | moodStatus = FailureMoodStatus
                        , moodList = []
                      }
                    , Cmd.none
                    )


{-| Helper function for `update`. Updates the form and returns Cmd.none.
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model
        | form = transform model.form
      }
    , Cmd.none
    )



---- VIEW ----


view : Model -> Document Msg
view model =
    let
        content =
            div []
                [ viewForm model.form
                , Html.hr [] []
                , lazy viewMoodSelector model
                , Html.hr [] []
                , viewMoodDetails model.currentMood
                , Html.hr [] []
                , viewMoodDashboard model
                ]
    in
    { title = "Elm Niconico app"
    , body =
        [ lazy viewHeader model.page
        , content
        , viewFooter
        ]
    }


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Elm Niconico" ]

        links =
            ul []
                [ navLink Login "Login"
                , navLink Moods "Moods"
                ]

        navLink : Page -> String -> Html Msg
        navLink targetPage caption =
            li [ classList [ ( "active", page == targetPage ) ] ]
                [ p [ onClick (ChangedPage targetPage) ] [ text caption ] ]
    in
    nav [] [ logo, links ]


viewFooter : Html msg
viewFooter =
    footer [] [ text "One is never alone with a rubber duck. - Douglas Adams" ]


viewForm : Form -> Html Msg
viewForm form =
    Html.form [ onSubmit SubmittedForm ]
        [ input
            [ onInput EnterUsername
            , placeholder "Username"
            ]
            []
        , input
            [ onInput EnterPassword
            , type_ "password"
            , placeholder "Password"
            ]
            []
        , button [] [ text "Sign in" ]
        ]


viewLoginForm : Model -> Html Msg
viewLoginForm model =
    div []
        [ input [] [] ]


viewMoodSelector : Model -> Html Msg
viewMoodSelector model =
    div []
        [ div []
            [ h1 [ onClick (SelectMood Happy) ]
                [ i
                    [ class "far fa-smile-beam fa-2x"
                    , class "mood_icon"
                    , classList [ ( "selected", model.currentMoodRating == Happy ) ]
                    ]
                    []
                ]
            , h1 [ onClick (SelectMood Neutral) ]
                [ i
                    [ class "far fa-meh fa-2x"
                    , class "mood_icon"
                    , classList [ ( "selected", model.currentMoodRating == Neutral ) ]
                    ]
                    []
                ]
            , h1 [ onClick (SelectMood Bad) ]
                [ i
                    [ class "far fa-sad-tear fa-2x"
                    , class "mood_icon"
                    , classList [ ( "selected", model.currentMoodRating == Bad ) ]
                    ]
                    []
                ]
            ]
        , div []
            [ div []
                [ p [] [ text "How do you feel?" ]
                , input
                    [ value model.currentInput
                    , onInput UpdateCurrentInput
                    ]
                    []
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

        moodText mood =
            mood.moodComment

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
                , title [] [ Svg.text (moodText mood) ]
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



---- HTTP ----


{-| TODO:

  - [ ] TrimmedForm
  - [ ] input validation

-}
login : Form -> Encode.Value
login form =
    Encode.object
        [ ( "username", Encode.string form.username )
        , ( "password", Encode.string form.password )
        ]



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
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
