module UserFeed exposing (Model, Msg, init, update, view)

import Feed
import Html exposing (Html)


type alias Model =
    Feed.Model


type alias Msg =
    Feed.Msg


-- START:feedUrl
feedUrl : String -> String
feedUrl username =
    "https://programming-elm.com/user/" ++ username ++ "/feed"
-- END:feedUrl


-- START:init
init : String -> ( Model, Cmd Msg )
init username =
    Feed.init
        { feedUrl = feedUrl username
        , wsUrl = Nothing
        }
-- END:init


view : Model -> Html Msg
view =
    Feed.view


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    Feed.update
