-- START:module
module PublicFeed exposing (Model, Msg, init, subscriptions, update, view)
-- END:module

-- START:import
import Feed
import Html exposing (Html)
-- END:import


-- START:type.aliases
type alias Model =
    Feed.Model


type alias Msg =
    Feed.Msg
-- END:type.aliases


-- START:urls
feedUrl : String
feedUrl =
    "https://programming-elm.com/feed"


wsUrl : String
wsUrl =
    "wss://programming-elm.com/"
-- END:urls


-- START:init
init : ( Model, Cmd Msg )
init =
    Feed.init
        { feedUrl = feedUrl
        , wsUrl = Just wsUrl
        }
-- END:init


-- START:view.update.subscriptions
view : Model -> Html Msg
view =
    Feed.view


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    Feed.update


subscriptions : Model -> Sub Msg
subscriptions =
    Feed.subscriptions
-- END:view.update.subscriptions
