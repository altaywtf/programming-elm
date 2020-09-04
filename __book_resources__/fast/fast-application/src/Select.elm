module Select exposing (Selection(..), filterBy, view, viewWithAll)

import Html exposing (Html, option, select, text)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (on, targetValue)
import Json.Decode as Json exposing (Decoder)
import Regex


type Selection a
    = All
    | One a


type IncludeAll
    = IncludeAll
    | NoAll


type alias SelectOneOrAllConfig a msg =
    { serialize : a -> String
    , display : a -> String
    , decode : String -> Decoder a
    , selection : Selection a
    , options : List a
    , onSelect : Selection a -> msg
    }


type alias SelectOneConfig a msg =
    { serialize : a -> String
    , display : a -> String
    , decode : String -> Decoder a
    , selection : a
    , options : List a
    , onSelect : a -> msg
    }


filterBy : (a -> b) -> Selection b -> List a -> List a
filterBy attribute selection values =
    case selection of
        All ->
            values

        One selectedValue ->
            List.filter (\value -> attribute value == selectedValue) values


viewOptions : (Selection a -> String) -> (a -> String) -> Selection a -> List a -> List (Html msg)
viewOptions serialize display selection options =
    List.map
        (\optionValue ->
            option
                [ selected (selection == One optionValue)
                , value (serialize (One optionValue))
                ]
                [ text (display optionValue) ]
        )
        options


viewAllOption : (Selection a -> String) -> Selection a -> Html msg
viewAllOption serialize selection =
    option
        [ selected (selection == All)
        , value (serialize All)
        ]
        [ text "All" ]


viewHelp : IncludeAll -> SelectOneOrAllConfig a msg -> Html msg
viewHelp includeAll config =
    let
        serialize =
            serializeSelection config.serialize

        options =
            viewOptions serialize config.display config.selection config.options
    in
    select [ onSelect (decodeSelection config.decode) config.onSelect ] <|
        case includeAll of
            IncludeAll ->
                viewAllOption serialize config.selection :: options

            NoAll ->
                options


viewWithAll : SelectOneOrAllConfig a msg -> Html msg
viewWithAll config =
    let
        serialize =
            serializeSelection config.serialize

        options =
            viewOptions serialize config.display config.selection config.options
    in
    select [ onSelect (decodeSelection config.decode) config.onSelect ] <|
        viewAllOption serialize config.selection
            :: options


view : SelectOneConfig a msg -> Html msg
view config =
    let
        serialize =
            serializeSelection config.serialize

        selection =
            One config.selection
    in
    select [ onSelect (decodeOne config.decode) config.onSelect ] <|
        viewOptions serialize config.display selection config.options


decodeSelection : (String -> Decoder a) -> String -> Decoder (Selection a)
decodeSelection decodeInner selection =
    case selection of
        "all" ->
            Json.succeed All

        _ ->
            selection
                |> decodeOne decodeInner
                |> Json.map One


decodeOne : (String -> Decoder a) -> String -> Decoder a
decodeOne decodeInner selection =
    let
        parsed =
            Regex.fromString "^one\\((.*)\\)$"
                |> Maybe.map
                    (\oneRegex ->
                        selection
                            |> Regex.find oneRegex
                            |> List.concatMap .submatches
                    )
    in
    case parsed of
        Just ((Just inner) :: []) ->
            decodeInner inner

        _ ->
            Json.fail "Unknown selection"


serializeSelection : (a -> String) -> Selection a -> String
serializeSelection serializeInner selection =
    case selection of
        All ->
            "all"

        One value ->
            "one(" ++ serializeInner value ++ ")"


onSelect : (String -> Decoder a) -> (a -> msg) -> Html.Attribute msg
onSelect decode tagger =
    let
        decoder =
            targetValue
                |> Json.andThen decode
                |> Json.map tagger
    in
    on "input" decoder
