module PhotoFolders exposing (Model, init, initialModel, modelDecoder)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Model =
    { selectedPhotoUrl : Maybe String
    }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , modelDecoder
        |> Http.get "http://elm-in-action.com/folders/list"
        |> Http.send LoadPage
    )


modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed initialModel


type Msg
    = SelectPhotoUrl String
    | LoadPage (Result Http.Error Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPhotoUrl url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        LoadPage (Ok newModel) ->
            ( newModel, Cmd.none )

        LoadPage (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    h1 [] [ text "The Grooviest Folders the world has ever seen" ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
