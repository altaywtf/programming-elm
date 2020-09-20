module Main exposing (main)

import Account
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (Html, a, div, h1, i, text)
import Html.Attributes exposing (class)
import PublicFeed
import Routes
import Url exposing (Url)
import UserFeed
import WebSocket


type Page
    = PublicFeed PublicFeed.Model
    | UserFeed String UserFeed.Model
    | Account Account.Model
    | NotFound



---- MODEL ----


type alias Model =
    { page : Page
    , navigationKey : Navigation.Key
    }


initialModel : Navigation.Key -> Model
initialModel navigationKey =
    { page = NotFound
    , navigationKey = navigationKey
    }


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url navigationKey =
    setNewPage (Routes.match url) (initialModel navigationKey)



---- VIEW ----


viewHeader : Html Msg
viewHeader =
    div [ class "header" ]
        [ div [ class "header-nav" ]
            [ a [ class "nav-brand", Routes.href Routes.Home ] [ text "Picshare" ]
            , a [ class "nav-account", Routes.href Routes.Account ] [ i [ class "fa fa-2x fa-gear" ] [] ]
            ]
        ]


viewContent : Page -> ( String, Html Msg )
viewContent page =
    case page of
        PublicFeed publicFeedModel ->
            ( "Picshare"
            , PublicFeed.view publicFeedModel
                |> Html.map PublicFeedMsgWrapper
            )

        UserFeed username userFeedModel ->
            ( "User Feed for @" ++ username
            , UserFeed.view userFeedModel |> Html.map UserFeedMsgWrapper
            )

        Account accountModel ->
            ( "Account"
            , Account.view accountModel
                |> Html.map AccountMsgWrapper
            )

        NotFound ->
            ( "Not found", div [ class "not-found" ] [ h1 [] [ text "Page not found" ] ] )


view : Model -> Document Msg
view model =
    let
        ( title, content ) =
            viewContent model.page
    in
    { title = title
    , body = [ viewHeader, content ]
    }



---- UPDATE ----


type Msg
    = NewRoute (Maybe Routes.Route)
    | Visit UrlRequest
    | AccountMsgWrapper Account.Msg
    | PublicFeedMsgWrapper PublicFeed.Msg
    | UserFeedMsgWrapper UserFeed.Msg


processPageUpdate :
    (pageModel -> Page)
    -> (pageMsg -> Msg)
    -> Model
    -> ( pageModel, Cmd pageMsg )
    -> ( Model, Cmd Msg )
processPageUpdate createPage wrapMsg model ( pageModel, pageCmd ) =
    ( { model | page = createPage pageModel }
    , Cmd.map wrapMsg pageCmd
    )


setNewPage : Maybe Routes.Route -> Model -> ( Model, Cmd Msg )
setNewPage maybeRoute model =
    case maybeRoute of
        Just Routes.Home ->
            PublicFeed.init
                |> processPageUpdate PublicFeed PublicFeedMsgWrapper model

        Just Routes.Account ->
            Account.init
                |> processPageUpdate Account AccountMsgWrapper model

        Just (Routes.UserFeed username) ->
            UserFeed.init username
                |> processPageUpdate (UserFeed username) UserFeedMsgWrapper model

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( NewRoute maybeRoute, _ ) ->
            let
                ( updatedModel, cmd ) =
                    setNewPage maybeRoute model
            in
            ( updatedModel
            , Cmd.batch [ cmd, WebSocket.close () ]
            )

        ( Visit (Browser.Internal url), _ ) ->
            ( model, Navigation.pushUrl model.navigationKey (Url.toString url) )

        ( PublicFeedMsgWrapper publicFeedMsg, PublicFeed publicFeedModel ) ->
            PublicFeed.update publicFeedMsg publicFeedModel
                |> processPageUpdate PublicFeed PublicFeedMsgWrapper model

        ( AccountMsgWrapper accountMsg, Account accountModel ) ->
            Account.update accountMsg accountModel
                |> processPageUpdate Account AccountMsgWrapper model

        ( UserFeedMsgWrapper userFeedMsg, UserFeed username userFeedModel ) ->
            UserFeed.update userFeedMsg userFeedModel
                |> processPageUpdate (UserFeed username) UserFeedMsgWrapper model

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        PublicFeed publicFeedModel ->
            PublicFeed.subscriptions publicFeedModel
                |> Sub.map PublicFeedMsgWrapper

        _ ->
            Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = Visit
        , onUrlChange = Routes.match >> NewRoute
        }
