module Main exposing (..)

import Browser
import String exposing ( fromInt, concat, split, toInt, trim, fromFloat )
import String.Extra exposing (..)
import Html exposing (Html, text, div, h1, img, input, p, textarea, button)
import Html.Attributes exposing (..)
import Array exposing (initialize, toList, length)
import List.Extra exposing (..)
import Result.Extra exposing (..)
import Html.Events exposing (onInput, onClick)
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
type Instruction = L | R | F | Unknown
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
        

type alias RawPosition = String
type alias RawInstructions = String

letterToInstr : String -> Result ParseError Instruction
letterToInstr l =
    case l of
        "L" -> Ok L
        "R" -> Ok R
        "F" -> Ok F
        _ -> Err (concat ["Unknown command ",l])

letterToOrientation : String -> Result ParseError Orientation
letterToOrientation l =
    case l of
        "E" -> Ok E
        "W" -> Ok W
        "S" -> Ok S
        "N" -> Ok N
        _ -> Err (concat ["Unknown orientation ", l])

parseRobot : RawPosition -> RawInstructions -> Result ParseError (Robot, List Instruction)
parseRobot p instr =
    let
        coord = split " " p
        instructions = List.map letterToInstr (split "" instr)
    in case coord of
        x::y::[c] -> if (List.all isOk instructions) then
            case letterToOrientation c of
                Err e -> Err e
                Ok orient -> Ok (
                    (Robot
                        { x = (Maybe.withDefault 0 (String.toInt x)),
                        y = (Maybe.withDefault 0 (String.toInt y)) } -- TODO: Proper err handling
                        orient),
                    (List.map (extract (\_ -> Unknown)) instructions))
            else
                Err (concat ["Some instructions were invalid in ", instr])
        _ -> Err (concat ["Unable to parse a robot from ", p])


parseRobotsR: List String -> List (Robot, List Instruction) -> List (Robot, List Instruction)
parseRobotsR str rs = case str of
    [] -> rs 
    [x] -> rs -- TODO: spit out error here instead 
    pos::instr::rest -> case parseRobot pos instr of
        Ok robotAndInstructions -> parseRobotsR rest (rs ++ [robotAndInstructions])
        Err e -> rs


parseRobots: List String -> List (Robot, List Instruction)
parseRobots strs =
    let
        nonEmpty = List.filter (\el -> not ( String.isEmpty el )) strs
    in
        parseRobotsR nonEmpty []

validateInput : String -> Input
validateInput s = case parseMars s of
    Err err -> Err err
    Ok (mars, otherLines) ->
       Ok (ValidInput mars (parseRobots otherLines))

-- type ValidInput = ValidInput Mars (List (Robot, List Instruction))
-- type alias Input = Result ParseError ValidInput
type alias Model = {
    rawInput: String,
    input: Input
    }

init : ( Model, Cmd Msg )
init =
    let
        data = """
        5 3
        1 1 E
        RFRFRFRF

        3 2 N
        FRRFLLFRRFLL

        0 3 W
        LLFFFLFLFL
        """
    in ({
        rawInput = data,
        input = validateInput data
    },
    Cmd.none )

step : ValidInput -> (ValidInput, Msg)
step (ValidInput ((Mars botLeft topRight movedRobots) as m) robsAndInstructs) = case robsAndInstructs of
    [] -> ((ValidInput m []), End)
    (r, ins)::xs -> case ins of
        [] ->
            let
                newStaticRobots = movedRobots ++ [Present r]
                newMars = Mars botLeft topRight newStaticRobots
                newWs = ValidInput newMars xs
            in
                (newWs, None)
        [i] ->
            let
                newStaticRobots = movedRobots ++ [robotStep m r i]
                newMars = Mars botLeft topRight newStaticRobots
                newWs = ValidInput newMars xs
            in
                (newWs, None)
        i::is -> case robotStep m r i of
            Lost lr ->
                let
                    newStaticRobots = movedRobots ++ [Lost lr]
                    newMars = Mars botLeft topRight newStaticRobots
                    newWs = ValidInput newMars xs
                in
                    (newWs, None)
            Present pr ->
                let
                    newWs = ValidInput m ((pr, is)::xs)
                in
                    (newWs, None)

robotStep : Mars -> Robot -> Instruction -> MovedRobot
robotStep m r i =
    let
        ((Robot newCoords newOrientation) as newR) = blindInstructionFollow r i
    in
        if (isInMars m newCoords) then
            Present newR
        else
            Lost newR

blindInstructionFollow : Robot -> Instruction -> Robot
blindInstructionFollow (Robot c orient) i = case (i, orient) of
    (F, N) -> Robot { c | y = c.y - 1 } N
    (F, S) -> Robot { c | y = c.y + 1 } S
    (F, W) -> Robot { c | x = c.x - 1 } W
    (F, E) -> Robot { c | x = c.x + 1 } E
    (L, N) -> Robot c E
    (L, S) -> Robot c W
    (L, W) -> Robot c N
    (L, E) -> Robot c S
    (R, N) -> Robot c W
    (R, S) -> Robot c E
    (R, W) -> Robot c S
    (R, E) -> Robot c N
    (Unknown, _) -> Robot c orient

isInMars : Mars -> Coord -> Bool
isInMars (Mars tr bl _) c = List.all identity [
    bl.x <= c.x, c.x <= tr.x,
    bl.y <= c.y, c.y <= tr.y
    ]

---- UPDATE ----


type Msg
    = NewInput String
    | TakeStep
    | End
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    NewInput str -> ( { model | rawInput = str, input = validateInput str }, Cmd.none )
    TakeStep -> case model.input of
        Err _ -> (model, Cmd.none)
        Ok (ValidInput mars instructions) ->
            let
                inp = model.input
                (newWs, _) = step (ValidInput mars instructions)
            in
                ({ model | input = Ok newWs }, Cmd.none)
    End -> (model, Cmd.none)
    None -> (model, Cmd.none)


---- VIEW ----
type alias Col = Int

type alias MaxCol = Int

type alias Row = Int

getMarsSquare : Row -> MaxCol -> Col -> Html msg
getMarsSquare r total n = div [
    style "background-color" "lightred",
    attribute "class" "mars-tile",
    style "border" "1px orange solid"
    ] [text ((fromInt n) ++ "," ++ (fromInt r))]

getMarsLine : MaxCol -> Row -> Int -> Int -> Html msg
getMarsLine sqCount r maxR _ = div [
    style "width" "100%",
    style "display" "flex",
    style "flex-flow" "row wrap"
    ] (List.map (getMarsSquare (abs (maxR - r)) sqCount) (toList (Array.initialize sqCount identity)))

getOrientationCls : Orientation -> String
getOrientationCls o =
    let
        base = "orientation orientation--"
    in
    case o of
        N -> base ++ "n"
        S -> base ++ "s"
        W -> base ++ "w"
        E -> base ++ "e"

getPosInPercentage : MarsTopRight -> Coord -> { left: Float, bottom: Float }
getPosInPercentage tr c =
    let
        flTrX = toFloat (tr.x + 1)
        flTrY = toFloat (tr.y + 1)
        flCX = toFloat c.x
        flCY = toFloat c.y
    in
        {
            left = (flCX / flTrX ) * 100,
            bottom = (flCY / flTrY ) * 100 -- 0 0 is 100% 0 for css
        }


getRobot : MarsTopRight -> Int -> Robot -> Html msg
getRobot tr n (Robot coord orientation) =
    let
        pos = getPosInPercentage tr coord 
    in
        div [
            attribute "class" "robot",
            attribute "class" (getOrientationCls orientation),
            style "bottom" (concat [fromFloat pos.bottom, "%"]), 
            style "left" (concat [fromFloat pos.left, "%"]) 
            ] [ text ( "R-" ++ (fromInt (n + 1)))]

-- tr 5 3
-- c 1 1 -> top 20% left 33%
-- c 0 3 -> 
getMars : Mars -> List Robot -> Html msg
getMars (Mars tr bl _) lr = div[
    style "width" (concat [fromInt (50 * tr.x), "px"]),
    style "height" (concat [fromInt (50 * tr.y), "px"]),
    style "position" "absolute",
    style "display" "flex",
    style "flex-flow" "row wrap",
    style "top" "0",
    style "bottom" "0",
    style "left" "0",
    style "right" "0",
    style "margin" "auto"
    ] (
       List.indexedMap (getMarsLine (tr.x + 1) (tr.y)) (toList (Array.initialize (tr.y + 1) identity)) ++
       List.indexedMap (getRobot tr) lr
    )
    
getForm : Model -> Html Msg
getForm model = div [] [
    textarea [ value model.rawInput, onInput NewInput ] [],

    (case model.input of
        Err e ->
            div [] [text e]

        Ok validInput ->
            text ""
    ),
    button [onClick TakeStep ] [ text "Step" ]
    ]

view : Model -> Html Msg
view model =
    div [] [
        getForm model,
        case model.input of
            Err e ->
                div [] []

            Ok (ValidInput m r) ->
                div [] [
                    getMars m (List.map (\(rob, ins) -> rob) r)
                ]
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
