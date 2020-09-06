module Picshare exposing (main)

import Browser
import Html exposing (Html, button, div, form, h1, h2, i, img, input, li, strong, text, ul)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, bool, decodeString, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import WebSocket


baseUrl : String
baseUrl =
    "https://programming-elm.com/"


wsUrl : String
wsUrl =
    "wss://programming-elm.com/"


type alias Id =
    Int


type alias Photo =
    { id : Id
    , url : String
    , caption : String
    , liked : Bool
    , comments : List String
    , newComment : String
    }


type alias Feed =
    List Photo


type alias Model =
    { feed : Maybe Feed
    , error : Maybe Http.Error
    , streamQueue : Feed
    }


type Msg
    = ToggleLike Id
    | UpdateComment Id String
    | SaveComment Id
    | LoadFeed (Result Http.Error Feed)
    | LoadStreamPhoto (Result Json.Decode.Error Photo)
    | FlushStreamQueue



-- Decoders


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "id" int
        |> required "url" string
        |> required "caption" string
        |> required "liked" bool
        |> required "comments" (list string)
        |> hardcoded ""



-- Model


initialModel : Model
initialModel =
    { feed = Nothing, error = Nothing, streamQueue = [] }



-- Commands


fetchFeed : Cmd Msg
fetchFeed =
    Http.get
        { url = baseUrl ++ "feed"
        , expect = Http.expectJson LoadFeed (list photoDecoder)
        }



-- Update


toggleLike : Photo -> Photo
toggleLike photo =
    { photo | liked = not photo.liked }


updateComment : String -> Photo -> Photo
updateComment newComment photo =
    { photo | newComment = newComment }


saveNewComment : Photo -> Photo
saveNewComment photo =
    let
        formattedComment =
            String.trim photo.newComment
    in
    case formattedComment of
        "" ->
            photo

        _ ->
            { photo
                | comments = photo.comments ++ [ formattedComment ]
                , newComment = ""
            }


updatePhotoById : (Photo -> Photo) -> Id -> Feed -> Feed
updatePhotoById updatePhoto id feed =
    List.map
        (\photo ->
            if photo.id == id then
                updatePhoto photo

            else
                photo
        )
        feed


updateFeed : (Photo -> Photo) -> Id -> Maybe Feed -> Maybe Feed
updateFeed updatePhoto id maybeFeed =
    Maybe.map (updatePhotoById updatePhoto id) maybeFeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleLike id ->
            ( { model | feed = updateFeed toggleLike id model.feed }, Cmd.none )

        UpdateComment id newComment ->
            ( { model | feed = updateFeed (updateComment newComment) id model.feed }, Cmd.none )

        SaveComment id ->
            ( { model | feed = updateFeed saveNewComment id model.feed }, Cmd.none )

        LoadFeed (Ok feed) ->
            ( { model | feed = Just feed }, WebSocket.listen wsUrl )

        LoadFeed (Err error) ->
            ( { model | error = Just error }, Cmd.none )

        LoadStreamPhoto (Ok photo) ->
            ( { model | streamQueue = photo :: model.streamQueue }, Cmd.none )

        LoadStreamPhoto (Err _) ->
            ( model, Cmd.none )

        FlushStreamQueue ->
            ( { model
                | feed = Maybe.map ((++) model.streamQueue) model.feed
                , streamQueue = []
              }
            , Cmd.none
            )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.receive (LoadStreamPhoto << decodeString photoDecoder)



-- View


viewLoveButton : Photo -> Html Msg
viewLoveButton photo =
    let
        buttonClass =
            if photo.liked then
                "fa-heart"

            else
                "fa-heart-o"
    in
    div [ class "like-button" ]
        [ i
            [ class "fa fa-2x"
            , class buttonClass
            , onClick (ToggleLike photo.id)
            ]
            []
        ]


viewComment : String -> Html Msg
viewComment comment =
    li []
        [ strong [] [ text "Comment:" ]
        , text (" " ++ comment)
        ]


viewCommentList : List String -> Html Msg
viewCommentList comments =
    case comments of
        [] ->
            text ""

        _ ->
            div [ class "comments" ]
                [ ul [] (List.map viewComment comments)
                ]


viewNewCommentForm : Photo -> Html Msg
viewNewCommentForm photo =
    form [ class "new-comment", onSubmit (SaveComment photo.id) ]
        [ input
            [ type_ "text"
            , placeholder "Add a comment..."
            , value photo.newComment
            , onInput (UpdateComment photo.id)
            ]
            []
        , button
            [ disabled (String.isEmpty photo.newComment) ]
            [ text "Save" ]
        ]


viewComments : Photo -> Html Msg
viewComments photo =
    div []
        [ viewCommentList photo.comments
        , viewNewCommentForm photo
        ]


viewDetailedPhoto : Photo -> Html Msg
viewDetailedPhoto photo =
    div [ class "detailed-photo" ]
        [ img [ src photo.url ] []
        , div [ class "photo-info" ]
            [ viewLoveButton photo
            , h2 [ class "caption" ] [ text photo.caption ]
            , viewComments photo
            ]
        ]


viewFeedLoadingState : Html Msg
viewFeedLoadingState =
    div [ class "loading-feed" ]
        [ text "Loading Feed..." ]


viewFeedEmptyState : Html Msg
viewFeedEmptyState =
    div [ class "loading-feed" ]
        [ text "No photos :(" ]


viewFeed : Maybe Feed -> Html Msg
viewFeed maybeFeed =
    case maybeFeed of
        Just feed ->
            case feed of
                [] ->
                    viewFeedEmptyState

                _ ->
                    div [] (List.map viewDetailedPhoto feed)

        Nothing ->
            viewFeedLoadingState


viewFeedErrorState : Http.Error -> Html Msg
viewFeedErrorState error =
    let
        errorMessage =
            case error of
                Http.BadBody _ ->
                    "Bad Body 😟"

                _ ->
                    "Oops, an error occurred 😉"
    in
    div [ class "feed-error" ]
        [ text errorMessage ]


viewStreamNotification : Feed -> Html Msg
viewStreamNotification queue =
    case queue of
        [] ->
            text ""

        _ ->
            let
                content =
                    "View new photos: " ++ String.fromInt (List.length queue)
            in
            div [ class "stream-notification", onClick FlushStreamQueue ] [ text content ]


viewContent : Model -> Html Msg
viewContent model =
    case model.error of
        Just error ->
            viewFeedErrorState error

        Nothing ->
            div []
                [ viewStreamNotification model.streamQueue
                , viewFeed model.feed
                ]


view : Model -> Html Msg
view model =
    div
        []
        [ div [ class "header" ]
            [ h1 [] [ text "Picshare" ] ]
        , div [ class "content-flow" ]
            [ viewContent model ]
        ]



-- Main


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, fetchFeed )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
