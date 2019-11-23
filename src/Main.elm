module Main exposing (..)

import Browser
import String exposing ( fromInt, concat, split, toInt, trim )
import String.Extra exposing (..)
import Html exposing (Html, text, div, h1, img, input, p, textarea)
import Html.Attributes exposing (..)
import Array exposing (initialize, toList, length)
import List.Extra exposing (..)
import Html.Events exposing (onInput)
import Debug exposing (log)



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
type alias ParseError = String
type ValidInput = ValidInput Mars (List (Robot, List Instruction))
type alias Input = Result ParseError ValidInput

move : Mars -> MovedRobot -> Instruction -> Mars
move m r ins = m

-- validate and parse Mars, return the remainder of the string as a remainder of the tuple
parseMars : String -> Result ParseError (Mars, List String)
parseMars rawS =
    let
        s = trim rawS
        list = List.map trim (split "\n" s)
        x = List.head list
        firstLine = List.filter (\l -> l /= "")  (split " " (Maybe.withDefault "" x))
    in
    if (List.length firstLine) == 2 then
        let
            coordX = List.head firstLine
            coordY = last firstLine
        in
            case (coordX, coordY) of
            (Just a, Just b) ->
                let
                    xInt = toInt a
                    yInt = toInt b
                in
                case (xInt, yInt) of
                    (Just xi, Just yi) ->
                        Ok (
                            (Mars { x = xi, y = yi } { x = 0, y = 0} []),
                            (Maybe.withDefault [] (List.tail list))
                        )
                    _ -> Err "Unable to parse map corner coordinates as numbers"
            _ -> Err "Unable to extract two coordinates from the first line"
    else
        Err "Please have the first line as 'A B' where A and B are numbers"
        

validateInput : String -> Input
validateInput s = case parseMars s of
    Err err -> Err err
    Ok (mars, otherLines) ->
       Ok (ValidInput mars [])

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
    ] (List.map (getMarsSquare sqCount) (toList (Array.initialize sqCount identity)))

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
       List.map (getMarsLine tr.x) (toList (Array.initialize tr.x identity))
    )
    
getForm : Model -> Html Msg
getForm model = div [] [
    input [ value model.rawInput, onInput NewInput ] [],

    (case model.input of
        Err e ->
            div [] [text e]

        Ok validInput ->
            text ""
    )]

view : Model -> Html Msg
view model =
    div [] [
        getForm model,
        case model.input of
            Err e ->
                div [] []

            Ok (ValidInput m _) ->
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
