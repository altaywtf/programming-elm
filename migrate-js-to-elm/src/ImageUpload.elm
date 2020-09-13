port module ImageUpload exposing (main)

import Browser
import Html exposing (Html, div, img, input, label, li, text, ul)
import Html.Attributes exposing (class, for, id, multiple, src, type_, width)
import Html.Events exposing (on)
import Json.Decode exposing (succeed)


onChange : msg -> Html.Attribute msg
onChange msg =
    on "change" (succeed msg)


type alias Flags =
    { imageUploaderId : String
    }


type alias Image =
    { url : String
    }


port uploadImages : () -> Cmd msg


port receiveImages : (List Image -> msg) -> Sub msg


type alias Model =
    { images : List Image
    , imageUploaderId : String
    }


type Msg
    = UploadImages
    | ReceiveImages (List Image)


viewImage : Image -> Html msg
viewImage image =
    li [ class "image-upload__image" ]
        [ img
            [ src image.url, width 400 ]
            []
        ]


view : Model -> Html Msg
view model =
    div [ class "image-upload" ]
        [ label [ for model.imageUploaderId ] [ text "+ Add Images" ]
        , input
            [ id model.imageUploaderId
            , type_ "file"
            , multiple True
            , onChange UploadImages
            ]
            []
        , ul [ class "image-upload__images" ] (List.map viewImage model.images)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UploadImages ->
            ( model, uploadImages () )

        ReceiveImages images ->
            ( { model | images = images }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveImages ReceiveImages


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model [] flags.imageUploaderId, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
