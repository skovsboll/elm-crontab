module HumanizerTest exposing (sunshine)

import Cron
import Expect
import Humanizer
import Parser
import Test exposing (Test, describe, only, test)


sunshine : Test
sunshine =
    describe "humanizer"
        [ describe "numerical expressions"
            [ test "stars" <|
                \() ->
                    Expect.equal
                        "every minute."
                        (explain "* * * * *")
            , test "proper time of day" <|
                \() ->
                    Expect.equal
                        "at 10:30."
                        (explain "30 10 * * *")
            , test "proper time of day, leading zeroes" <|
                \() ->
                    Expect.equal
                        "at 09:05."
                        (explain "5 9 * * *")
            , test "hours only" <|
                \() ->
                    Expect.equal
                        "every minute past 10."
                        (explain "* 10 * * *")
            , test "minutes only" <|
                \() ->
                    Expect.equal
                        "at 30 minutes past every hour."
                        (explain "30 * * * *")
            , test "stroke of" <|
                \() ->
                    Expect.equal
                        "at the stroke of every hour."
                        (explain "0 * * * *")
            , test "stroke of with range" <|
                \() ->
                    Expect.equal
                        "at the stroke of 1 through 2 o'clock."
                        (explain "0 1-2 * * *")
            , test "minutes - no padding" <|
                \() ->
                    Expect.equal
                        "at 1 minute past every hour."
                        (explain "1 * * * *")
            , test "day of month" <|
                \() ->
                    Expect.equal
                        "every minute, on the third day of the month."
                        (explain "* * 3 * *")
            , test "month" <|
                \() ->
                    Expect.equal
                        "every minute, in January."
                        (explain "* * * 1 *")
            , test "day of week" <|
                \() ->
                    Expect.equal
                        "every minute, on Sunday."
                        (explain "* * * * 0")
            ]
        , describe "multiple"
            [ test "days of months" <|
                \() ->
                    Expect.equal
                        "every minute, on the first day of the month and on the third day of the month and on the fourth day of the month."
                        (explain "* * 1,3,4 * *")
            ]
        , describe "simplifications"
            [ test "all five" <|
                \() ->
                    Expect.equal
                        "at 02:01, on the third day of the month, in April, on Friday."
                        (explain "1 2 3 4 5")
            , test "first four" <|
                \() ->
                    Expect.equal
                        "at 02:01, on the third day of the month, in April."
                        (explain "1 2 3 4 *")
            , test "first three" <|
                \() ->
                    Expect.equal
                        "at 02:01, on the third day of the month."
                        (explain "1 2 3 * *")
            , test "first two" <|
                \() ->
                    Expect.equal
                        "at 02:01."
                        (explain "1 2 * * *")
            , test "first one" <|
                \() ->
                    Expect.equal
                        "at 1 minute past every hour."
                        (explain "1 * * * *")
            ]
        , describe "steps"
            [ test "stars" <|
                \() ->
                    Expect.equal
                        "every second minute past every third hour, every fourth day of the month, every fifth month, every sixth day of the week."
                        (explain "*/2 */3 */4 */5 */6")
            , test "step ordinals" <|
                \() ->
                    Expect.equal
                        "every 11th minute past every hour."
                        (explain "*/11 * * * *")
            , test "steps and start" <|
                \() ->
                    Expect.equal
                        "every second minute from 1 through 59 past every third hour from 2 through 23, every fourth day of the month from 3 through 31, every fifth month from April through December, every sixth day of the week from Friday through Saturday."
                        (explain "1/2 2/3 3/4 4/5 5/6")
            , describe "ranges"
                [ test "single" <|
                    \() ->
                        Expect.equal
                            "from 15 through 20 minutes past 2 through 3 o'clock, from the third through the fourth day of the month, from May through June, from Sunday through Wednesday."
                            (explain "15-20 2-3 3-4 5-6 0-3")
                ]
            , test "multiple" <|
                \() ->
                    Expect.equal
                        "from 1 through 2 minutes and from 5 through 6 minutes past 2 through 3 o'clock and 6 through 7 o'clock, from the third through the fourth day of the month and from the eight through the ninth day of the month, from May through June and in January and in April, from Sunday through Wednesday and from Friday through Saturday."
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
