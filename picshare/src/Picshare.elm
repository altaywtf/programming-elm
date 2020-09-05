module Picshare exposing (main)

import Browser
import Html exposing (Html, button, div, form, h1, h2, i, img, input, li, strong, text, ul)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, bool, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)


baseUrl : String
baseUrl =
    "https://programming-elm.com/"


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


type alias Model =
    { photo : Maybe Photo }


type Msg
    = ToggleLike
    | UpdateComment String
    | SaveComment
    | LoadFeed (Result Http.Error Photo)



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
    { photo = Nothing }



-- Commands


fetchFeed : Cmd Msg
fetchFeed =
    Http.get
        { url = baseUrl ++ "feed/1"
        , expect = Http.expectJson LoadFeed photoDecoder
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


updateFeed : (Photo -> Photo) -> Maybe Photo -> Maybe Photo
updateFeed updatePhoto maybePhoto =
    Maybe.map updatePhoto maybePhoto


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleLike ->
            ( { model | photo = updateFeed toggleLike model.photo }, Cmd.none )

        UpdateComment newComment ->
            ( { model | photo = updateFeed (updateComment newComment) model.photo }, Cmd.none )

        SaveComment ->
            ( { model | photo = updateFeed saveNewComment model.photo }, Cmd.none )

        LoadFeed (Ok photo) ->
            ( { model | photo = Just photo }, Cmd.none )

        LoadFeed (Err _) ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
            , onClick ToggleLike
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


viewNewCommentForm : String -> Html Msg
viewNewCommentForm newComment =
    form [ class "new-comment", onSubmit SaveComment ]
        [ input
            [ type_ "text"
            , placeholder "Add a comment..."
            , value newComment
            , onInput UpdateComment
            ]
            []
        , button
            [ disabled (String.isEmpty newComment) ]
            [ text "Save" ]
        ]


viewComments : Photo -> Html Msg
viewComments photo =
    div []
        [ viewCommentList photo.comments
        , viewNewCommentForm photo.newComment
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


viewFeedEmptyState : Html Msg
viewFeedEmptyState =
    div [ class "loading-feed" ]
        [ text "Loading Feed..." ]


viewFeed : Maybe Photo -> Html Msg
viewFeed maybePhoto =
    case maybePhoto of
        Just photo ->
            viewDetailedPhoto photo

        Nothing ->
            viewFeedEmptyState


view : Model -> Html Msg
view model =
    div
        []
        [ div [ class "header" ]
            [ h1 [] [ text "Picshare" ] ]
        , div [ class "content-flow" ]
            [ viewFeed model.photo ]
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
