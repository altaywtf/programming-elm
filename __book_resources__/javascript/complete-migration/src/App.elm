port module App exposing (main)

import Browser
import Html exposing (Html, div, h2, h3, img, input, label, li, pre, text, textarea, ul)
import Html.Attributes exposing (class, for, id, multiple, src, type_, value, width)
import Html.Events exposing (on, onInput)
import Json.Decode as Decode exposing (Decoder, decodeValue, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


onChange : msg -> Html.Attribute msg
onChange msg =
    on "change" (succeed msg)


type alias Flags =
    { imageUploaderId : String
    , note : Note
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    succeed Flags
        |> required "imageUploaderId" string
        |> required "note" noteDecoder


type alias Image =
    { url : String }


imagesDecoder : Decoder (List (Maybe Image))
imagesDecoder =
    let
        imageDecoder =
            succeed Image
                |> required "url" string
    in
    list (maybe imageDecoder)


type alias Note =
    { title : String
    , contents : String
    , images : List (Maybe Image)
    }


noteDecoder : Decoder Note
noteDecoder =
    succeed Note
        |> required "title" string
        |> required "contents" string
        |> required "images" imagesDecoder


port uploadImages : () -> Cmd msg


port saveNote : Note -> Cmd msg


port receiveImages : (Decode.Value -> msg) -> Sub msg


type alias Model =
    { imageUploaderId : String
    , note : Note
    }


type alias ModelResult =
    Result Decode.Error Model


init : Decode.Value -> ( ModelResult, Cmd Msg )
init flagsValue =
    case decodeValue flagsDecoder flagsValue of
        Ok flags ->
            ( Ok (Model flags.imageUploaderId flags.note)
            , Cmd.none
            )

        Err error ->
            ( Err error, Cmd.none )


viewImage : Maybe Image -> Html Msg
viewImage maybeImage =
    case maybeImage of
        Just image ->
            li [ class "image-upload__image" ]
                [ img
                    [ src image.url
                    , width 400
                    ]
                    []
                ]

        Nothing ->
            li [ class "image-upload__image" ]
                [ h3 [ class "image-upload__error" ]
                    [ text "Can't display image" ]
                ]


viewImageUpload : Model -> Html Msg
viewImageUpload model =
    div [ class "image-upload" ]
        [ label [ for model.imageUploaderId ]
            [ text "+ Add Images" ]
        , input
            [ id model.imageUploaderId
            , type_ "file"
            , multiple True
            , onChange UploadImages
            ]
            []
        , ul [ class "image-upload__images" ]
            (List.map viewImage model.note.images)
        ]


viewModel : Model -> Html Msg
viewModel model =
    div [ class "note" ]
        [ div [ class "note__info" ]
            [ h2 [] [ text "Info" ]
            , div [ class "note__title" ]
                [ label [] [ text "Title:" ]
                , input
                    [ type_ "text"
                    , value model.note.title
                    , onInput UpdateTitle
                    ]
                    []
                ]
            , div [ class "note__contents" ]
                [ label [] [ text "Contents:" ]
                , textarea
                    [ value model.note.contents
                    , onInput UpdateContents
                    ]
                    []
                ]
            ]
        , div [ class "note__images" ]
            [ h2 [] [ text "Images" ]
            , viewImageUpload model
            ]
        ]


view : ModelResult -> Html Msg
view modelResult =
    case modelResult of
        Ok model ->
            viewModel model

        Err error ->
            div [ class "note-load-error" ]
                [ h2 []
                    [ text "Problem initializing application from localStorage" ]
                , pre []
                    [ text (Decode.errorToString error) ]
                ]


updateTitle : String -> Note -> Note
updateTitle title note =
    { note | title = title }


updateContents : String -> Note -> Note
updateContents contents note =
    { note | contents = contents }


addImages : List (Maybe Image) -> Note -> Note
addImages images note =
    { note | images = note.images ++ images }


updateNote : (Note -> Note) -> Model -> ( Model, Cmd msg )
updateNote updater model =
    let
        newNote =
            updater model.note
    in
    ( { model | note = newNote }
    , saveNote newNote
    )


type Msg
    = UploadImages
    | ReceiveImages (Result Decode.Error (List (Maybe Image)))
    | UpdateTitle String
    | UpdateContents String


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
    case msg of
        UploadImages ->
            ( model, uploadImages () )

        ReceiveImages (Ok images) ->
            updateNote (addImages images) model

        ReceiveImages (Err _) ->
            ( model, Cmd.none )

        UpdateTitle title ->
            updateNote (updateTitle title) model

        UpdateContents contents ->
            updateNote (updateContents contents) model


update : Msg -> ModelResult -> ( ModelResult, Cmd Msg )
update msg modelResult =
    case Result.map (updateModel msg) modelResult of
        Ok ( model, cmd ) ->
            ( Ok model, cmd )

        Err _ ->
            ( modelResult, Cmd.none )


subscriptions : ModelResult -> Sub Msg
subscriptions modelResult =
    receiveImages (ReceiveImages << decodeValue imagesDecoder)


main : Program Decode.Value ModelResult Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
