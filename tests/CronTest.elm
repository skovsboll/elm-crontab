module CronTest exposing (..)

import Cron exposing (Atom(..), Cron(..), Expr(..), Term(..))
import Expect exposing (Expectation, fail, pass)
import Parser exposing (DeadEnd, Problem(..))
import Test exposing (..)


sunshine : Test
sunshine =
    describe "sunshine"
        [ describe "numerical expressions"
            [ test "lots of spaces" <|
                \() ->
                    Expect.equal
                        (Ok (Cron (Single (Simple (Numeric 0))) (Single (Simple (Numeric 12))) (Single (Simple (Numeric 1))) (Single (Simple (Numeric 1))) (Single (Simple (Numeric 2)))))
                        (Cron.fromString "    0   12    1 1 2   ")
            , test "zero prefixed" <|
                \() ->
                    Expect.equal
                        (Ok (Cron (Single (Simple (Numeric 0))) (Single (Simple (Numeric 12))) (Single (Simple (Numeric 1))) (Single (Simple (Numeric 1))) (Single (Simple (Numeric 2)))))
                        (Cron.fromString "00 012 01 01 02")
            ]
        , describe "stars"
            [ test "all stars" <|
                \() ->
                    Expect.equal
                        (Ok (Cron Every Every Every Every Every))
                        (Cron.fromString "    *   * *   *  *  ")
            , test "combinations" <|
                \() ->
                    Expect.equal
                        (Ok (Cron Every (Single (Simple (Numeric 1))) Every (Single (Simple (Numeric 1))) Every))
                        (Cron.fromString "    *   1 *   1  *  ")
            ]
        , describe "steps"
            [ test "every step" <|
                \() ->
                    Expect.equal
                        (Ok (Cron Every Every Every Every (Single (EveryStep 1))))
                        (Cron.fromString "* * * * */1")
            , test "every 1 step" <|
                \() ->
                    Expect.equal
                        (Ok (Cron Every Every Every Every (Single (Step (Numeric 1) 1))))
                        (Cron.fromString "* * * * 1/1")
            , test "every third step" <|
                \() ->
                    Expect.equal
                        (Ok (Cron Every Every Every Every (Single (EveryStep 3))))
                        (Cron.fromString "* * * * */3")
            , test "every third step on the 2nd" <|
                \() ->
                    Expect.equal
                        (Ok (Cron Every Every Every Every (Single (Step (Numeric 2) 3))))
                        (Cron.fromString "* * * * 2/3")
            , test "every third step on the second to fourth" <|
                \() ->
                    Expect.equal
                        (Ok (Cron Every Every Every Every (Single (Step (Range 2 4) 3))))
                        (Cron.fromString "* * * * 2-4/3")
            ]
        , describe "ranges"
            [ test "single range" <|
                \() ->
                    Expect.equal
                        (Ok (Cron (Single (Simple (Range 1 2))) Every Every Every Every))
                        (Cron.fromString "1-2 * * * *")
            , test "all ranges" <|
                \() ->
                    Expect.equal
                        (Ok (Cron (Single (Simple (Range 1 2))) (Single (Simple (Range 1 2))) (Single (Simple (Range 1 2))) (Single (Simple (Range 1 2))) (Single (Simple (Range 1 2)))))
                        (Cron.fromString "1-2 1-2 1-2 1-2 1-2")
            , test "combinations" <|
                \() ->
                    Expect.equal
                        (Ok (Cron Every (Single (Simple (Range 2 5))) (Single (Simple (Range 1 2))) (Single (Simple (Numeric 1))) (Single (Simple (Range 1 2)))))
                        (Cron.fromString "* 2-5 1-2 1 1-2")
            ]
        , describe "sequences"
            [ test "sequence of literals" <|
                \() ->
                    Expect.equal
                        (Ok (Cron (Multiple [ Simple (Numeric 1), Simple (Numeric 2) ]) Every Every Every Every))
                        (Cron.fromString "1,2 * * * *")
            , test "sequence of ranges and literals" <|
                \() ->
                    Expect.equal
                        (Ok (Cron (Multiple [ Simple (Numeric 1), Simple (Range 2 4) ]) Every Every Every Every))
                        (Cron.fromString "1,2-4 * * * *")
            , test "sequence of ranges, literals and steps" <|
                \() ->
                    Expect.equal
                        (Ok (Cron (Multiple [ Simple (Numeric 1), Step (Range 2 4) 3 ]) Every Every Every Every))
                        (Cron.fromString "1,2-4/3 * * * *")
            ]
        ]


rain : Test
rain =
    describe "rain"
        [ describe "combinations"
            [ test "named week days not supported" <|
                \() ->
                    expectFirstProblem
                        (Problem "not a valid int")
                        (Cron.fromString "* * * * SUN")
            , test "named months not supported" <|
                \() ->
                    expectFirstProblem
                        (Problem "not a valid int")
                        (Cron.fromString "* * FEB * 1")
            ]
        , describe "out of range"
            [ test "minutes outside range" <|
                \() ->
                    expectFirstProblem
                        (Problem "minutes, the first number, is 61. I was expecting values in the range from 0 to 59.")
                        (Cron.fromString "61 0 0 0 0")
            , test "hours outside range" <|
                \() ->
                    expectFirstProblem
                        (Problem "hours, the second number, is 25. I was expecting values in the range from 0 to 23.")
                        (Cron.fromString "0 25 0 0 0")
            , test "DOM outside range" <|
                \() ->
                    expectFirstProblem
                        (Problem "day of month, the third number, is 32. I was expecting values in the range from 1 to 31.")
                        (Cron.fromString "0 0 32 0 0")
            , test "month outside range" <|
                \() ->
                    expectFirstProblem
                        (Problem "month, the fourth number, is 13. I was expecting values in the range from 1 to 12.")
                        (Cron.fromString "0 0 1 13 0")
            , test "DOW outside range" <|
                \() ->
                    expectFirstProblem
                        (Problem "day of week, the fifth number, is 10. I was expecting values in the range from 0 to 6.")
                        (Cron.fromString "0 0 3 1 10")
            ]
        , describe "ranges"
            [ test "out of range" <|
                \() ->
                    expectFirstProblem
                        (Problem "day of week, the fifth number, is 7. I was expecting values in the range from 0 to 6.")
                        (Cron.fromString "* 2-5 1-2 1 7-8")
            , test "double range" <|
                \() ->
                    expectFirstProblem
                        (Problem "not a valid int")
                        (Cron.fromString "* * 1-2-3 * *")
            ]
        , describe "steps"
            [ test "every 0 step" <|
                \() ->
                    expectFirstProblem
                        (Problem "A step value of 0 was seen. Step values must be 1 or higher.")
                        (Cron.fromString "* * * * 1/0")
            , test "Multiple steps" <|
                \() ->
                    expectFirstProblem
                        (Problem "not a valid int")
                        (Cron.fromString "* * * 1-2/1-3 *")
            , test "Multiple steps at the end" <|
                \() ->
                    expectFirstProblem
                        ExpectingEnd
                        (Cron.fromString "* * * * 1-2/1-3")
            ]
        , describe "sequences"
            [ test "out of range" <|
                \() ->
                    expectFirstProblem
                        (Problem "day of month, the third number, is 32. I was expecting values in the range from 1 to 31.")
                        (Cron.fromString "1,2 * 30,31,32 * *")
            ]
        ]



-----------------------------------------------
-- Helpers
-----------------------------------------------


expectFirstProblem : Problem -> Result (List DeadEnd) Cron -> Expectation
expectFirstProblem problem result =
    case result of
        Ok cron ->
            fail ("Parsing was expected to fail with " ++ Debug.toString problem ++ " but it returned " ++ Debug.toString cron)

        Err (head :: _) ->
            Expect.equal head.problem problem

        Err [] ->
            fail "No problems returned"
