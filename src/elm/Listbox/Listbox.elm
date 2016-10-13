port module Listbox exposing (listbox, items, onSelectedChanged, setSelected)

import Dict exposing (Dict)
import Maybe exposing (Maybe)
import Json.Decode as Decode
import Json.Encode as Encode
import Html exposing (Attribute, Html, text, button, span, div, node)
import Html.Attributes exposing (attribute, class, property)
import Html.Events exposing (on, onClick, onMouseOver, onMouseOut)
import Html.App exposing (program)


-- The exposed API


port setSelected : List ( String, String ) -> Cmd msg


listbox : List (Attribute msg) -> Html msg
listbox attrs =
    Html.node "wood-listbox" attrs []


items : Dict String String -> Attribute msg
items val =
    property "items" <| (encodeItems (Dict.toList val))


onItemArrayChanged : String -> (Dict String String -> msg) -> Attribute msg
onItemArrayChanged propname tagger =
    on propname <| Decode.map tagger <| Decode.map Dict.fromList decodeItems


onSelectedChanged : (Dict String String -> msg) -> Attribute msg
onSelectedChanged tagger =
    onItemArrayChanged "select-changed" tagger


onItemsChanged : (Dict String String -> msg) -> Attribute msg
onItemsChanged tagger =
    onItemArrayChanged "items-changed" tagger


encodeItems : List ( String, String ) -> Encode.Value
encodeItems items =
    Encode.list
        (List.map (\( idx, val ) -> Encode.list [ Encode.string idx, Encode.string val ]) items)


decodeItems : Decode.Decoder (List ( String, String ))
decodeItems =
    Decode.at [ "detail", "value" ] <| Decode.list <| Decode.tuple2 (,) Decode.string Decode.string



-- The internals


port itemsChanged : (List ( String, String ) -> msg) -> Sub msg


type alias Model =
    { items : Dict String String
    , selectedItems : Dict String String
    , hoverItem : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { items = Dict.empty
      , selectedItems = Dict.empty
      , hoverItem = Nothing
      }
    , Cmd.none
    )


woodItem : List (Attribute msg) -> List (Html msg) -> Html msg
woodItem attrs inner =
    node "wood-item" attrs inner


view : Model -> Html Msg
view model =
    div
        []
        (Dict.toList model.items |> List.map (itemsToList model))


isHover : String -> Model -> Bool
isHover idx model =
    model.hoverItem == Just idx


isSelected : String -> Model -> Bool
isSelected idx model =
    Dict.member idx model.selectedItems


itemsToList model ( idx, value ) =
    let
        hover =
            isHover idx model

        selected =
            isSelected idx model

        attrs =
            [ onMouseOver <| MouseOver idx
            , onMouseOut <| MouseOut idx
            , Html.Attributes.value (toString idx)
            ]

        selectAttrs =
            if selected then
                (onClick <| Deselect idx) :: attrs
            else
                (onClick <| Select idx) :: attrs

        styleAttrs =
            if hover && selected then
                class "wood-selected wood-highlight" :: selectAttrs
            else if hover && not selected then
                class "wood-highlight" :: selectAttrs
            else if not hover && selected then
                class "wood-selected" :: selectAttrs
            else
                selectAttrs
    in
        woodItem
            styleAttrs
            [ text value ]


type Msg
    = ItemsChanged (List ( String, String ))
    | Select String
    | Deselect String
    | MouseOver String
    | MouseOut String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "Listbox" msg) of
        ItemsChanged items ->
            ( { model | items = Dict.fromList items }, Cmd.none )

        Select idx ->
            case (Dict.get idx model.items) of
                Just value ->
                    let
                        newSelection =
                            Dict.insert idx value model.selectedItems
                    in
                        ( { model | selectedItems = newSelection }
                        , Dict.toList newSelection |> setSelected
                        )

                Nothing ->
                    ( model, Cmd.none )

        Deselect idx ->
            let
                newSelection =
                    Dict.remove idx model.selectedItems
            in
                ( { model | selectedItems = newSelection }
                , Dict.toList newSelection |> setSelected
                )

        MouseOver idx ->
            ( { model | hoverItem = Just idx }, Cmd.none )

        MouseOut idx ->
            ( { model | hoverItem = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    itemsChanged ItemsChanged


main : Program Never
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
