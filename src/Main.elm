module Main exposing (..)

import Browser
import String exposing ( fromInt, concat )
import Html exposing (Html, text, div, h1, img, input, p, textarea)
import Html.Attributes exposing (..)
import Array exposing (initialize, toList)
import Html.Events exposing (onInput)


---- MODEL ----

type alias Coord = {
    x: Int,
    y: Int
    }
type Orientation = E | W | S | N
type Robot = Robot Coord Orientation
type MovedRobot = Lost Robot | Present Robot
type alias MarsTopRight = Coord
type alias MarsBottomLeft = Coord
type Mars = Mars MarsTopRight MarsBottomLeft (List MovedRobot)
type Instruction = L | R | F
type ValidInput = ValidInput Mars (List Instruction)
type Input = Error String | Valid ValidInput

move : Mars -> MovedRobot -> Instruction -> Mars
move m r ins = m

validateInput : String -> Input
validateInput s = Valid (ValidInput
    (Mars
        {x = 5, y = 5 }
        {x = 0, y = 0 }
        []
        )
    [])

type alias Model = {
    rawInput: String,
    input: Input
    }

init : ( Model, Cmd Msg )
init = ({
        rawInput = "",
        input = validateInput ""
        },
        Cmd.none )



---- UPDATE ----


type Msg
    = NewInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    NewInput str -> ( { model | rawInput = str, input = validateInput str }, Cmd.none )


---- VIEW ----
getMarsSquare : Int -> Int -> Html msg
getMarsSquare total n = div [
    style "background-color" "lightred",
    style "height" "50px",
    style "flex-basis" (concat [fromInt (100 // total), "%"]),
    style "border" "1px orange solid"
    ] []

getMarsLine : Int -> Int -> Html msg
getMarsLine sqCount _ = div [
    style "width" "100%",
    style "display" "flex",
    style "flex-flow" "row wrap"
    ] (List.map (getMarsSquare sqCount) (toList (initialize sqCount identity)))

getMars : Mars -> Html msg
getMars (Mars tr bl _) = div[
    style "width" "300px",
    style "height" "300px",
    style "position" "absolute",
    style "top" "0",
    style "bottom" "0",
    style "left" "0",
    style "right" "0",
    style "margin" "auto"
    ] (
       List.map (getMarsLine tr.x) (toList (initialize tr.x identity))
    )
    
getForm : Model -> Html Msg
getForm model = div [] [
    input [ value model.rawInput ] [],

    (case model.input of
        Error e ->
            div [] [text e]

        Valid validInput ->
            text ""
    )]

view : Model -> Html Msg
view model =
    div [] [
        getForm model,
        case model.input of
            Error e ->
                div [] []

            Valid (ValidInput m _) ->
                getMars m
    ]

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
