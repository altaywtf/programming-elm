-- START:module
module Account exposing (Model, Msg, init, update, view)
-- END:module

import Html exposing (Html, button, div, h2, img, input, label, span, text, textarea)
import Html.Attributes exposing (class, disabled, for, id, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Json exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


accountUrl : String
accountUrl =
    "https://programming-elm.com/account"



---- MODEL ----


type SaveStatus
    = Unsaved
    | Saving
    | Success
    | Fail


type alias Account =
    { name : String
    , username : String
    , bio : String
    , avatarUrl : String
    }


type alias Model =
    { account : Maybe Account
    , error : Maybe Http.Error
    , saveStatus : SaveStatus
    }


accountDecoder : Decoder Account
accountDecoder =
    succeed Account
        |> required "name" string
        |> required "username" string
        |> required "bio" string
        |> required "avatarUrl" string


initialModel : Model
initialModel =
    { account = Nothing
    , error = Nothing
    , saveStatus = Unsaved
    }


-- START:init
init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchAccount )
-- END:init


fetchAccount : Cmd Msg
fetchAccount =
    Http.get
        { url = accountUrl
        , expect = Http.expectJson LoadAccount accountDecoder
        }



---- VIEW ----


type InputType
    = Input
    | TextArea


type alias InputRowConfig =
    { type_ : InputType
    , id : String
    , label : String
    , value : String
    , onInput : String -> Msg
    }


viewRow : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewRow attributes children =
    div (attributes ++ [ class "row" ]) children


viewInputRow : InputRowConfig -> Html Msg
viewInputRow config =
    let
        baseInputAttributes =
            [ id config.id
            , value config.value
            , onInput config.onInput
            ]

        inputField =
            case config.type_ of
                Input ->
                    input (type_ "text" :: baseInputAttributes) []

                TextArea ->
                    textarea baseInputAttributes []
    in
    viewRow []
        [ label [ for config.id ]
            [ text config.label ]
        , div []
            [ inputField ]
        ]


viewAccount : SaveStatus -> Account -> Html Msg
viewAccount saveStatus account =
    div [ class "account" ]
        [ viewRow [ class "row--header" ]
            [ img [ class "avatar", src account.avatarUrl ] []
            , h2 [ class "username" ] [ text account.username ]
            ]
        , viewInputRow
            { type_ = Input
            , id = "name"
            , label = "Name"
            , value = account.name
            , onInput = Update Name
            }
        , viewInputRow
            { type_ = Input
            , id = "username"
            , label = "Username"
            , value = account.username
            , onInput = Update Username
            }
        , viewInputRow
            { type_ = TextArea
            , id = "bio"
            , label = "Bio"
            , value = account.bio
            , onInput = Update Bio
            }
        , div [ class "actions" ]
            [ button
                [ disabled (saveStatus == Saving)
                , onClick Save
                ]
                [ text "Save" ]
            , viewSaveStatus saveStatus
            ]
        ]


viewSaveStatus : SaveStatus -> Html msg
viewSaveStatus saveStatus =
    let
        config =
            case saveStatus of
                Saving ->
                    Just ( "saving", "Saving..." )

                Success ->
                    Just ( "success", "Saved Successfully!" )

                Fail ->
                    Just ( "fail", "Error saving!" )

                _ ->
                    Nothing
    in
    case config of
        Just ( modifierClass, message ) ->
            span [ class "saved", class ("saved--" ++ modifierClass) ]
                [ text message ]

        Nothing ->
            text ""


view : Model -> Html Msg
view model =
    case model.account of
        Just account ->
            viewAccount model.saveStatus account

        Nothing ->
            div [ class "loading-account" ]
                [ text "Loading Account..." ]



---- UPDATE ----


type Field
    = Name
    | Username
    | Bio


type Msg
    = Update Field String
    | LoadAccount (Result Http.Error Account)
    | Save
    | SavedAccount (Result Http.Error ())


saveAccount : Account -> Cmd Msg
saveAccount account =
    let
        body =
            Http.jsonBody <|
                Encode.object
                    [ ( "name", Encode.string account.name )
                    , ( "username", Encode.string account.username )
                    , ( "bio", Encode.string account.bio )
                    , ( "avatarUrl", Encode.string account.avatarUrl )
                    ]

        decoder =
            accountDecoder
                |> Json.map (always ())
    in
    Http.request
        { method = "PUT"
        , headers = []
        , url = accountUrl
        , body = body
        , expect = Http.expectJson SavedAccount decoder
        , timeout = Nothing
        , tracker = Nothing
        }


updateAccount : Field -> String -> Account -> Account
updateAccount field value account =
    case field of
        Name ->
            { account | name = value }

        Username ->
            { account | username = value }

        Bio ->
            { account | bio = value }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update field value ->
            ( { model | account = Maybe.map (updateAccount field value) model.account }
            , Cmd.none
            )

        LoadAccount (Ok account) ->
            ( { model | account = Just account }
            , Cmd.none
            )

        LoadAccount (Err error) ->
            ( { model | error = Just error }
            , Cmd.none
            )

        SavedAccount (Ok _) ->
            ( { model | saveStatus = Success }
            , Cmd.none
            )

        SavedAccount (Err _) ->
            ( { model | saveStatus = Fail }
            , Cmd.none
            )

        Save ->
            ( { model | saveStatus = Saving }
            , Maybe.map saveAccount model.account
                |> Maybe.withDefault Cmd.none
            )
