module Main exposing (..)

import Html.Keyed as Keyed
import Browser
import Task
import String exposing ( fromInt, concat, split, toInt, trim, fromFloat )
import String.Extra exposing (..)
import Html exposing (Html, text, div, h1, img, input, p, textarea, button, pre)
import Html.Attributes exposing (..)
import Array exposing (initialize, toList, length)
import List.Extra exposing (..)
import Result.Extra exposing (..)
import Html.Events exposing (onInput, onClick)

---- MODEL ----

type alias Coord = {
    x: Int,
    y: Int
    }
type Orientation = E | W | S | N
type Robot = Robot Coord Orientation
type MovedRobot = Lost Robot Instruction | Present Robot
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
    input: Input,
    output: String
    }

init : ( Model, Cmd Msg )
init =
    let
        data = """
        5 3
        1 1 E
        RFRFRFRF

        3 2 N
        FRRFLLFFRRFLL

        0 3 W
        LLFFFLFLFL
        """
    in ({
        rawInput = data,
        input = validateInput data,
        output = ""
    },
    Cmd.none )

step : ValidInput -> (ValidInput, Cmd Msg)
step (ValidInput ((Mars botLeft topRight movedRobots) as m) robsAndInstructs) = case robsAndInstructs of
    [] -> ((ValidInput m []), Task.succeed End |> Task.perform identity)
    (r, ins)::xs -> case ins of
        [] ->
            let
                newStaticRobots = movedRobots ++ [Present r]
                newMars = Mars botLeft topRight newStaticRobots
                newWs = ValidInput newMars xs
            in
                (newWs, Cmd.none)
        [i] ->
            let
                newStaticRobots = movedRobots ++ [robotStep m r i]
                newMars = Mars botLeft topRight newStaticRobots
                newWs = ValidInput newMars xs
            in
                (newWs, Cmd.none)
        i::is -> case robotStep m r i of
            Lost lr lostInstruction ->
                let
                    newStaticRobots = movedRobots ++ [Lost lr lostInstruction]
                    newMars = Mars botLeft topRight newStaticRobots
                    newWs = ValidInput newMars xs
                in
                    (newWs, Cmd.none)
            Present pr ->
                let
                    newWs = ValidInput m ((pr, is)::xs)
                in
                    (newWs, Cmd.none)

robotStep : Mars -> Robot -> Instruction -> MovedRobot
robotStep ((Mars _ _ mr) as m) r i =
    let
        ((Robot newCoords newOrientation) as newR) = blindInstructionFollow r i
    in
        if (isInMars m newCoords) then
            Present newR
        else
            if someoneGotLostHereBefore mr r i then
                Present r
            else
                Lost r i


someoneGotLostHereBefore : List MovedRobot -> Robot -> Instruction -> Bool
someoneGotLostHereBefore lmr ((Robot { x, y } curRobotOr) as r) ii = case lmr of
    [] -> False
    head::tail -> case head of
        Lost (Robot prevRobotCoord o) prevRobotInstr ->
            if List.all identity [x == prevRobotCoord.x, y == prevRobotCoord.y, prevRobotInstr == ii, o == curRobotOr ] then
                True
            else
                someoneGotLostHereBefore tail r ii
        Present _ ->
                someoneGotLostHereBefore tail r ii

blindInstructionFollow : Robot -> Instruction -> Robot
blindInstructionFollow (Robot c orient) i = case (i, orient) of
    (F, N) -> Robot { c | y = c.y + 1 } N
    (F, S) -> Robot { c | y = c.y - 1 } S
    (F, W) -> Robot { c | x = c.x - 1 } W
    (F, E) -> Robot { c | x = c.x + 1 } E
    (L, N) -> Robot c W
    (L, S) -> Robot c E
    (L, W) -> Robot c S
    (L, E) -> Robot c N
    (R, N) -> Robot c E
    (R, S) -> Robot c W
    (R, W) -> Robot c N
    (R, E) -> Robot c S
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
    | Reset
    | StepToTheEnd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    NewInput str -> ( { model | rawInput = str, input = validateInput str }, Cmd.none )
    TakeStep -> case model.input of
        Err _ -> (model, Cmd.none)
        Ok (ValidInput mars instructions) ->
            let
                inp = model.input
                (newWs, cmd) = step (ValidInput mars instructions)
            in
                ({ model | input = Ok newWs }, cmd)
    End -> case model.input of
        Err _ -> (model, Cmd.none)
        Ok (ValidInput mars instructions) -> ({ model | output = getOutput mars }, Cmd.none)
    Reset -> ({
        rawInput = model.rawInput,
        input = validateInput model.rawInput,
        output = ""
        }, Cmd.none)
    StepToTheEnd ->
        let
            (newModel, cmd) = update TakeStep model
        in
            if cmd == Cmd.none then
                update StepToTheEnd newModel
            else
                (newModel, cmd)
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


getRobot : List (Html.Attribute msg) -> MarsTopRight -> Int -> Robot -> Html msg
getRobot attr tr n (Robot coord orientation) =
    let
        pos = getPosInPercentage tr coord 
    in
        Keyed.node "div" ([
            attribute "class" "robot",
            attribute "class" (getOrientationCls orientation),
            style "bottom" (concat [fromFloat pos.bottom, "%"]), 
            style "left" (concat [fromFloat pos.left, "%"]) 
            ]++attr) [(
                (fromInt coord.x),
                ( text ( "R-" ++ (fromInt (n + 1))))
            )]


getStoppedRobot : MarsTopRight -> Int -> MovedRobot -> Html msg
getStoppedRobot tr n mr =
    case mr of
        Lost r _ -> getRobot [
            attribute "class" "robot--lost"] tr n r
        Present r -> getRobot [
            attribute "class" "robot--stopped"] tr n r


getMars : Mars -> List Robot -> Html msg
getMars (Mars tr bl mrs) lr = div[
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
       List.indexedMap (getRobot [] tr) lr ++
       List.indexedMap (getStoppedRobot tr) mrs
    )
    
getForm : Model -> Html Msg
getForm model = div [] [
    textarea [ value model.rawInput, onInput NewInput ] [],

    (case model.input of
        Err e ->
            div [] [text e]

        Ok validInput ->
            text ""
    )
    ]

orientationToLetter : Orientation -> String
orientationToLetter o = case o of
    N -> "N"
    W -> "W"
    E -> "E"
    S -> "S"

printRobot : Robot -> String
printRobot (Robot coord o) = fromInt coord.x ++ " " ++ fromInt coord.y ++ " " ++orientationToLetter o ++ "\n"

printMovedRobot : MovedRobot -> String
printMovedRobot mr = case mr of
    Present r -> printRobot r
    Lost r _ -> printRobot r

getOutput : Mars -> String
getOutput (Mars _ _ movedRobots) = concat <| List.map printMovedRobot movedRobots

view : Model -> Html Msg
view model =
    div [] [
        getForm model,
        case model.input of
            Err e ->
                div [] []

            Ok (ValidInput m r) ->
                div [] [
                    getMars m (List.map (\(rob, ins) -> rob) r),
                    case model.output of
                        "" -> div [] [
                            button [onClick TakeStep ] [ text "Step" ],
                            button [onClick StepToTheEnd ] [ text "Just show results" ]]
                        output -> button [onClick Reset] [ text "Reset" ],
                    case model.output of
                        "" -> div [] []
                        output -> pre [ attribute "class" "output" ] [ text model.output ]
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
