module SaladBuilder exposing (main)

import Browser
import Html
    exposing
        ( Html
        , button
        , div
        , h1
        , h2
        , input
        , label
        , li
        , p
        , section
        , table
        , td
        , text
        , th
        , tr
        , ul
        )
import Html.Attributes exposing (checked, class, disabled, name, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Encode exposing (Value, list, object, string)
import Platform.Cmd exposing (Cmd)
import Regex
import Set exposing (Set)



---- MODEL ----


type alias Error =
    String


type Base
    = Lettuce
    | Spinach
    | SpringMix


baseToString : Base -> String
baseToString base =
    case base of
        Lettuce ->
            "Lettuce"

        Spinach ->
            "Spinach"

        SpringMix ->
            "Spring Mix"


type Topping
    = Tomatoes
    | Cucumbers
    | Onions


toppingToString : Topping -> String
toppingToString topping =
    case topping of
        Tomatoes ->
            "Tomatoes"

        Cucumbers ->
            "Cucumbers"

        Onions ->
            "Onions"


type Dressing
    = NoDressing
    | Italian
    | RaspberryVinaigrette
    | OilVinegar


dressingToString : Dressing -> String
dressingToString dressing =
    case dressing of
        NoDressing ->
            "No Dressing"

        Italian ->
            "Italian"

        RaspberryVinaigrette ->
            "Raspberry Vinaigrette"

        OilVinegar ->
            "Oil and Vinegar"


type alias Salad =
    { base : Base
    , toppings : Set String
    , dressing : Dressing
    }


type alias Contact c =
    { c
        | name : String
        , email : String
        , phone : String
    }


type alias Model =
    { building : Bool
    , sending : Bool
    , success : Bool
    , error : Maybe String

    --- ch6. nested state ---
    , salad : Salad

    --- ch6. extensible records --
    , name : String
    , email : String
    , phone : String
    }


initialModel : Model
initialModel =
    { building = True
    , sending = False
    , success = False
    , error = Nothing
    , salad =
        { base = Lettuce
        , toppings = Set.empty
        , dressing = NoDressing
        }
    , name = ""
    , email = ""
    , phone = ""
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, Cmd.none )



---- VALIDATION ----


isRequired : String -> Bool
isRequired value =
    String.trim value /= ""


isValidEmail : String -> Bool
isValidEmail value =
    let
        options =
            { caseInsensitive = True
            , multiline = False
            }

        regexString =
            "^(([^<>()\\[\\]\\.,;:\\s@\"]+(\\.[^<>()\\[\\]\\.,;:\\s@\"]+)*)|(\".+\"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}])|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$"

        regex =
            Regex.fromStringWith options regexString
                |> Maybe.withDefault Regex.never
    in
    value
        |> String.trim
        |> Regex.contains regex


isValidPhone : String -> Bool
isValidPhone value =
    let
        regex =
            Regex.fromString "^\\d{10}$"
                |> Maybe.withDefault Regex.never
    in
    value
        |> String.trim
        |> Regex.contains regex


isValid : Model -> Bool
isValid model =
    [ isRequired model.name
    , isRequired model.email
    , isValidEmail model.email
    , isRequired model.phone
    , isValidPhone model.phone
    ]
        |> List.all identity



---- VIEW ----


viewSending : Html msg
viewSending =
    div [ class "sending" ] [ text "Sending Order..." ]


viewError : Maybe Error -> Html msg
viewError maybeError =
    case maybeError of
        Just errorMessage ->
            div [ class "error" ] [ text errorMessage ]

        Nothing ->
            text ""


viewSection : String -> List (Html msg) -> Html msg
viewSection heading children =
    section [ class "salad-section" ]
        (h2 [] [ text heading ] :: children)


viewToppingOption : Set String -> Topping -> Html Msg
viewToppingOption toppings topping =
    label [ class "select-option" ]
        [ input
            [ type_ "checkbox"
            , checked (Set.member (toppingToString topping) toppings)
            , onCheck (SaladMsgWrapper << ToggleTopping topping)
            ]
            []
        , text (toppingToString topping)
        ]


viewSelectToppings : Set String -> Html Msg
viewSelectToppings toppings =
    div [] (List.map (viewToppingOption toppings) [ Tomatoes, Cucumbers, Onions ])


viewRadioOption : String -> value -> (value -> msg) -> String -> value -> Html msg
viewRadioOption radioName selectedValue tagger optionLabel value =
    label [ class "select-option" ]
        [ input
            [ type_ "radio"
            , name radioName
            , checked (selectedValue == value)
            , onClick (tagger value)
            ]
            []
        , text optionLabel
        ]


viewSelectBase : Base -> Html Msg
viewSelectBase currentBase =
    let
        viewBaseOption value =
            viewRadioOption "base" currentBase (SaladMsgWrapper << SetBase) (baseToString value) value
    in
    div [] (List.map viewBaseOption [ Lettuce, Spinach, SpringMix ])


viewSelectDressing : Dressing -> Html Msg
viewSelectDressing currentDressing =
    let
        viewDressingOption value =
            viewRadioOption "dressing" currentDressing (SaladMsgWrapper << SetDressing) (dressingToString value) value
    in
    div [] (List.map viewDressingOption [ NoDressing, Italian, RaspberryVinaigrette, OilVinegar ])


viewTextInput : String -> String -> (String -> msg) -> Html msg
viewTextInput inputLabel inputValue tagger =
    div [ class "text-input" ]
        [ label []
            [ div [] [ text inputLabel ]
            , input
                [ type_ "text"
                , value inputValue
                , onInput tagger
                ]
                []
            ]
        ]


viewContactForm : Contact a -> Html ContactMsg
viewContactForm contact =
    div []
        [ viewTextInput "Name" contact.name SetName
        , viewTextInput "Email" contact.email SetEmail
        , viewTextInput "Phone" contact.phone SetPhone
        ]


viewBuilding : Model -> Html Msg
viewBuilding model =
    div []
        [ viewError model.error
        , viewSection "1. Select Base" [ viewSelectBase model.salad.base ]
        , viewSection "2. Select Toppings" [ viewSelectToppings model.salad.toppings ]
        , viewSection "3. Select Dressing" [ viewSelectDressing model.salad.dressing ]
        , viewSection "4. Enter Contact Info"
            [ Html.map ContactMsgWrapper (viewContactForm model)
            , button
                [ class "send-button"
                , disabled (not (isValid model))
                , onClick Send
                ]
                [ text "Send Order" ]
            ]
        ]


viewConfirmation : Model -> Html msg
viewConfirmation model =
    div [ class "confirmation" ]
        [ h2 [] [ text "Woo hoo!" ]
        , p [] [ text "Thanks for your order!" ]
        , table []
            [ tr []
                [ th [] [ text "Base:" ]
                , td [] [ text (baseToString model.salad.base) ]
                ]
            , tr []
                [ th [] [ text "Toppings:" ]
                , td []
                    [ ul []
                        (model.salad.toppings
                            |> Set.toList
                            |> List.map (\topping -> li [] [ text topping ])
                        )
                    ]
                ]
            , tr []
                [ th [] [ text "Dressing:" ]
                , td [] [ text (dressingToString model.salad.dressing) ]
                ]
            , tr []
                [ th [] [ text "Name:" ]
                , td [] [ text model.name ]
                ]
            , tr []
                [ th [] [ text "Email:" ]
                , td [] [ text model.email ]
                ]
            , tr []
                [ th [] [ text "Phone:" ]
                , td [] [ text model.phone ]
                ]
            ]
        ]


viewStep : Model -> Html Msg
viewStep model =
    if model.sending then
        viewSending

    else if model.building then
        viewBuilding model

    else
        viewConfirmation model


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "header" ]
            [ text "Saladise - Build a Salad" ]
        , div [ class "content" ]
            [ viewStep model ]
        ]



---- UPDATE ----


type SaladMsg
    = SetBase Base
    | ToggleTopping Topping Bool
    | SetDressing Dressing


type ContactMsg
    = SetName String
    | SetEmail String
    | SetPhone String


type Msg
    = SaladMsgWrapper SaladMsg
    | ContactMsgWrapper ContactMsg
    | Send
    | SubmissionResult (Result Http.Error String)


sendUrl : String
sendUrl =
    "https://programming-elm.com/salad/send"


encodeOrder : Model -> Value
encodeOrder model =
    object
        [ ( "base", string (baseToString model.salad.base) )
        , ( "toppings", list string (Set.toList model.salad.toppings) )
        , ( "dressing", string (dressingToString model.salad.dressing) )
        , ( "name", string model.name )
        , ( "email", string model.email )
        , ( "phone", string model.phone )
        ]


send : Model -> Cmd Msg
send model =
    Http.post
        { url = sendUrl
        , body = Http.jsonBody (encodeOrder model)
        , expect = Http.expectString SubmissionResult
        }


updateSalad : SaladMsg -> Salad -> Salad
updateSalad msg salad =
    case msg of
        SetBase base ->
            { salad | base = base }

        ToggleTopping topping add ->
            let
                updater =
                    if add then
                        Set.insert

                    else
                        Set.remove
            in
            { salad | toppings = updater (toppingToString topping) salad.toppings }

        SetDressing dressing ->
            { salad | dressing = dressing }


updateContact : ContactMsg -> Contact c -> Contact c
updateContact msg contact =
    case msg of
        SetName name ->
            { contact | name = name }

        SetEmail email ->
            { contact | email = email }

        SetPhone phone ->
            { contact | phone = phone }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaladMsgWrapper saladMsg ->
            ( { model | salad = updateSalad saladMsg model.salad }, Cmd.none )

        ContactMsgWrapper contactMsg ->
            ( updateContact contactMsg model, Cmd.none )

        Send ->
            let
                newModel =
                    { model
                        | building = False
                        , sending = True
                        , error = Nothing
                    }
            in
            ( newModel
            , send newModel
            )

        SubmissionResult (Ok _) ->
            ( { model
                | sending = False
                , success = True
                , error = Nothing
              }
            , Cmd.none
            )

        SubmissionResult (Err _) ->
            ( { model
                | building = True
                , sending = False
                , error = Just "There was a problem sending your order. Please try again."
              }
            , Cmd.none
            )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
