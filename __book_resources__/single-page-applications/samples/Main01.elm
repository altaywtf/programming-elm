module Main exposing (main)

-- START:import.browser
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
-- END:import.browser
import Html exposing (Html, a, div, h1, i, text)
import Html.Attributes exposing (class)
-- START:import.routes.url
import Routes
import Url exposing (Url)
-- END:import.routes.url



---- MODEL ----


-- START:type.Page
type Page
    = PublicFeed
    | Account
    | NotFound
-- END:type.Page


-- START:model
type alias Model =
    { page : Page
    , navigationKey : Navigation.Key
    }


initialModel : Navigation.Key -> Model
initialModel navigationKey =
    { page = NotFound
    , navigationKey = navigationKey
    }
-- END:model


-- START:init
init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url navigationKey =
    setNewPage (Routes.match url) (initialModel navigationKey)
-- END:init



---- VIEW ----


-- START:viewContent
viewContent : Page -> ( String, Html Msg )
viewContent page =
    case page of
        PublicFeed ->
            ( "Picshare"
            , h1 [] [ text "Public Feed" ]
            )

        Account ->
            ( "Account"
            , h1 [] [ text "Account" ]
            )

        NotFound ->
            ( "Not Found"
            , div [ class "not-found" ]
                [ h1 [] [ text "Page Not Found" ] ]
            )
-- END:viewContent


-- START:view
view : Model -> Document Msg
view model =
    let
        ( title, content ) =
            viewContent model.page
    in
    { title = title
    , body = [ content ]
    }
-- END:view



---- UPDATE ----


-- START:type.Msg
type Msg
    = NewRoute (Maybe Routes.Route)
    | Visit UrlRequest
-- END:type.Msg


-- START:setNewPage
setNewPage : Maybe Routes.Route -> Model -> ( Model, Cmd Msg )
setNewPage maybeRoute model =
    case maybeRoute of
        Just Routes.Home ->
            ( { model | page = PublicFeed }, Cmd.none )

        Just Routes.Account ->
            ( { model | page = Account }, Cmd.none )

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )
-- END:setNewPage


-- START:update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRoute maybeRoute ->
            setNewPage maybeRoute model

        _ ->
            ( model, Cmd.none )
-- END:update


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- PROGRAM ----


main : Program () Model Msg
-- START:main
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = Visit
        , onUrlChange = Routes.match >> NewRoute
        }
-- END:main
