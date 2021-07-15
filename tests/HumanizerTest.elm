module HumanizerTest exposing (sunshine)

import Cron exposing (Atom(..), Cron(..), Expr(..), Term(..))
import Expect
import Humanizer
import Parser
import Test exposing (Test, describe, only, test)


sunshine : Test
sunshine =
    describe "sunshine"
        [ describe "numerical expressions"
            [ test "stars" <|
                \() ->
                    Expect.equal
                        "every minute, every hour, every day of the month, all year, all week."
                        (explain "* * * * *")
            , test "proper time of day" <|
                \() ->
                    Expect.equal
                        "at 10:30, every day of the month, all year, all week."
                        (explain "30 10 * * *")
            , test "proper time of day, leading zeroes" <|
                \() ->
                    Expect.equal
                        "at 09:05, every day of the month, all year, all week."
                        (explain "5 9 * * *")
            , test "hours only" <|
                \() ->
                    Expect.equal
                        "every minute, past 10, every day of the month, all year, all week."
                        (explain "* 10 * * *")
            , test "minutes only" <|
                \() ->
                    Expect.equal
                        "at 30 minutes past, every hour, every day of the month, all year, all week."
                        (explain "30 * * * *")
            , test "day of month" <|
                \() ->
                    Expect.equal
                        "every minute, every hour, on the third day of the month, all year, all week."
                        (explain "* * 3 * *")
            , test "month" <|
                \() ->
                    Expect.equal
                        "every minute, every hour, every day of the month, in January, all week."
                        (explain "* * * 1 *")
            , test "day of week" <|
                \() ->
                    Expect.equal
                        "every minute, every hour, every day of the month, all year, on Sunday."
                        (explain "* * * * 0")
            ]
        , describe "steps"
            [ test "stars" <|
                \() ->
                    Expect.equal
                        "every second minute, every third hour, every fourth day of the month, every fifth month, every sixth day of the week."
                        (explain "*/2 */3 */4 */5 */6")
            , test "steps and start" <|
                \() ->
                    Expect.equal
                        "every second minute from 1 through 59, every third hour from 2 through 23, every fourth day of the month from 3 through 31, every fifth month from April through December, every sixth day of the week from Friday through Saturday."
                        (explain "1/2 2/3 3/4 4/5 5/6")
            , describe "ranges"
                [ test "single" <|
                    \() ->
                        Expect.equal
                            "from 15 through 20 minutes past, from 2 through 3 o'clock, from the third through the fourth day of the month, from May through June, from Sunday through Wednesday."
                            (explain "15-20 2-3 3-4 5-6 0-3")
                ]
            , test "multiple" <|
                \() ->
                    Expect.equal
                        "from 1 through 2 minutes past and from 5 through 6 minutes past, from 2 through 3 o'clock and from 6 through 7 o'clock, from the third through the fourth day of the month and from the eight through the ninth day of the month, from May through June and in January and in April, from Sunday through Wednesday and from Friday through Saturday."
                        (explain "1-2,5-6 2-3,6-7 3-4,8-9 5-6,1,4 0-3,5-6")
            ]
        ]


explain : String -> String
explain cronSource =
    case cronSource |> Cron.fromString of
        Ok value ->
            Humanizer.toString value

        Err errors ->
            errors
                |> Parser.deadEndsToString
