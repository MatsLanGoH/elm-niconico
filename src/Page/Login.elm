module Page.Login exposing (view)

import Html exposing (Html, div, h1, main_, p, text)
import Html.Attributes exposing (class, id, tabindex)



-- VIEW


view : { title : String, content : Html msg }
view =
    { title = "Login"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Not Found" ]
            , div [ class "row" ]
                [ p [] [ text "Insert Login page here" ] ]
            ]
    }
