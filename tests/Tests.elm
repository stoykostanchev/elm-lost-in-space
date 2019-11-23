module Tests exposing (..)
import String exposing ( split )
import Test exposing (..)
import Main exposing(..)
import Expect

validateInputTest : Test
validateInputTest =
    describe "Input checks"
        [ test "Correct parsing of invalid input" <|
            \_ ->
                Expect.equal (validateInput "1 ") (Err "Please have the first line as 'A B' where A and B are numbers")
        , test "Correct parsing of mars coordinate" <|
            \_ ->
                Expect.equal
                  (parseMars """
                  1 1
                  1 2 E
                  RL

                  3 2 N
                  FRRL
                  """)
                  (Ok
                    ((Mars { x = 1, y = 1 } { x = 0, y = 0 } []),
                    ["1 2 E", "RL", "", "3 2 N", "FRRL"]))
        , test "Correct robot parsing" <|
            \_ ->
                Expect.equal
                    (parseRobots ["1 2 E", "RL", "", "3 2 N", "FRRL"])
                    [(
                        (Robot { x = 1, y = 2 } E),
                        [ R, L ]
                    ),
                    (
                        (Robot { x = 3, y = 2 } N),
                        [ F, R, R, L]
                    )]
        , test "Correct e2e parsing of a single robot" <|
            \_ ->
                Expect.equal
                  (Ok (ValidInput
                    (Mars { x = 5, y = 3 } { x = 0, y = 0 } [])
                    [(
                        (Robot { x = 1, y = 2 } E),
                        [ R, L, F ]
                    )])
                  )
                  (validateInput """
                  5 3
                  1 2 E
                  RLF
                  """)
        , test "Correct e2e parsing of multiple robots" <|
            \_ ->
                Expect.equal
                  (Ok (ValidInput
                    (Mars { x = 5, y = 3 } { x = 0, y = 0 } [])
                    [(
                        (Robot { x = 1, y = 2 } E),
                        [ R, L, F ]
                    ),
                    (
                        (Robot { x = 3, y = 5 } N),
                        [ L, L, L ]
                    )])
                  )
                  (validateInput """
                  5 3
                  1 2 E
                  RLF

                  3 5 N
                  LLL
                  """)

        ]