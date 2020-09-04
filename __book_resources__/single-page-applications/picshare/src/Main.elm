module Main exposing (main)

import Browser
import Html exposing (Html, a, div, h1, i, text)
import Html.Attributes exposing (class)



---- MODEL ----


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Single Page Applications" ] ]



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
