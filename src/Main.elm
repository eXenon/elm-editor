port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, h2, img, text)
import Html.Attributes exposing (contenteditable, id, property, src)
import Html.Events exposing (onClick)
import Html.Parser exposing (Node(..))
import Html.Parser.Util
import Json.Decode
import Json.Encode
import Markdown
import Regex


port sendStuff : Json.Encode.Value -> Cmd msg


port receiveStuff : (Json.Encode.Value -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { text : String, error : String, nodes : List (Html Msg) }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { text = "Edit me", error = "No error", nodes = [] }, Cmd.none )



---- TREE TYPES ----


toEditorText : List Node -> String
toEditorText nodes =
    case nodes of
        (Element "span" [ ( "id", "caret" ), ( "data-offset", offset ) ] _) :: siblings ->
            ""

        (Element "span" [ ( "id", "caret" ) ] _) :: siblings ->
            ""

        (Element tag attrs children) :: siblings ->
            if tag == "div" then
                toEditorText children ++ "\n" ++ toEditorText siblings

            else
                toEditorText children ++ toEditorText siblings

        (Text txt) :: siblings ->
            let
                surrounding_whitespace =
                    Maybe.withDefault Regex.never <| Regex.fromString "^\\s*|\\s$"
            in
            txt
                |> Regex.replace surrounding_whitespace (\_ -> "")

        (Comment _) :: siblings ->
            toEditorText siblings

        [] ->
            ""



---- UPDATE ----


type alias Flags =
    { value : String
    }


type Msg
    = SendData
    | Received (Result Json.Decode.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendData ->
            ( model, Cmd.none )

        Received result ->
            case result of
                Ok value ->
                    let
                        parsed =
                            Html.Parser.run value
                                |> Debug.log "Incoming HTML :"
                                |> Result.withDefault []

                        old =
                            parsed
                                |> Html.Parser.Util.toVirtualDom

                        new =
                            parsed
                                |> toEditorText
                                |> Debug.log "Extracted text :"
                                |> Markdown.toHtml Nothing
                    in
                    ( { model | text = value, nodes = new }, value |> Json.Encode.string |> sendStuff )

                Err error ->
                    ( { model | error = Json.Decode.errorToString error }, Cmd.none )


valueDecoder : Json.Decode.Decoder String
valueDecoder =
    Json.Decode.string



---- VIEW ----


view : Model -> Html Msg
view model =
    div [] model.nodes



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveStuff (Json.Decode.decodeValue valueDecoder >> Received)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
