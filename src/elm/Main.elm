module Main exposing (..)

import Set as Set
import Array
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (title, class, action, attribute)
import Html.Events exposing (on)
import Json.Decode as Decode
import Material.Options as Options exposing (Style, cs, when, nop, disabled)
import Material.Dialog as Dialog
import Material.Table as Table
import Material.Button as Button
import Material.Icon as Icon
import Material.Toggles as Toggles
import Material.Textfield as Textfield
import Material.Grid as Grid
import Material.Chip as Chip
import Listbox exposing (listbox, onSelectedChanged, items)
import Html exposing (..)
import Html.App exposing (..)


type alias Model =
    Dict String String


type Msg
    = SelectChanged (Dict String String)


root : Model -> Html Msg
root model =
    listbox
        [ items (Dict.fromList [ ( "1", "one" ), ( "2", "two" ), ( "3", "three" ) ])
        , onSelectedChanged SelectChanged
        ]


update : Msg -> Model -> Model
update msg model =
    case (Debug.log "Main" msg) of
        SelectChanged items ->
            items


main : Program Never
main =
    beginnerProgram
        { model = Dict.empty
        , view = root
        , update = update
        }
