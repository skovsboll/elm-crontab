module CronTest exposing (..)

import Cron exposing (Atom(..), Cron(..), Expr(..), Term(..))
import Expect exposing (Expectation, fail, pass)
import Parser exposing (Problem(..))
import Test exposing (..)


sunshine : Test
sunshine =
    describe "sunshine"
        [ describe "numerical expressions"
            [ test "lots of spaces" <|
                \() ->
                    Expect.equal
                        (Ok (Cron (Single (Simple (Literal 0))) (Single (Simple (Literal 12))) (Single (Simple (Literal 1))) (Single (Simple (Literal 1))) (Single (Simple (Literal 2)))))
                        (Cron.fromString "    0   12    1 1 2   ")
            , test "zero prefixed" <|
                \() ->
                    Expect.equal
                        (Ok (Cron (Single (Simple (Literal 0))) (Single (Simple (Literal 12))) (Single (Simple (Literal 1))) (Single (Simple (Literal 1))) (Single (Simple (Literal 2)))))
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
                        (Ok (Cron Every (Single (Simple (Literal 1))) Every (Single (Simple (Literal 1))) Every))
                        (Cron.fromString "    *   1 *   1  *  ")
            ]
        , describe "steps"
            [ test "every step" <|
                \() ->
                    Expect.equal
                        (Ok (Cron Every Every Every Every (Single (EveryStep 1))))
                        (Cron.fromString "* * * * */1")
            , test "every third step" <|
                \() ->
                    Expect.equal
                        (Ok (Cron Every Every Every Every (Single (EveryStep 3))))
                        (Cron.fromString "* * * * */3")
            , test "every third step on the 2nd" <|
                \() ->
                    Expect.equal
                        (Ok (Cron Every Every Every Every (Single (Step (Literal 2) 3))))
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
                        (Ok (Cron Every (Single (Simple (Range 2 5))) (Single (Simple (Range 1 2))) (Single (Simple (Literal 1))) (Single (Simple (Range 1 2)))))
                        (Cron.fromString "* 2-5 1-2 1 1-2")
            ]
        , describe "sequences"
            [ test "sequence of literals" <|
                \() ->
                    Expect.equal
                        (Ok (Cron (Multiple [ Simple (Literal 1), Simple (Literal 2) ]) Every Every Every Every))
                        (Cron.fromString "1,2 * * * *")
            , test "sequence of ranges and literals" <|
                \() ->
                    Expect.equal
                        (Ok (Cron (Multiple [ Simple (Literal 1), Simple (Range 2 4) ]) Every Every Every Every))
                        (Cron.fromString "1,2-4 * * * *")
            , test "sequence of ranges, literals and steps" <|
                \() ->
                    Expect.equal
                        (Ok (Cron (Multiple [ Simple (Literal 1), Step (Range 2 4) 3 ]) Every Every Every Every))
                        (Cron.fromString "1,2-4/3 * * * *")
            ]
        ]


rain : Test
rain =
    describe "rain"
        [ describe "combinations"
            [ test "named week days not supported" <|
                \() ->
                    case Cron.fromString "* * * * SUN" of
                        Err _ ->
                            pass

                        _ ->
                            fail "SUN supported"
            , test "named months not supported" <|
                \() ->
                    case Cron.fromString "* * FEB * 1" of
                        Err _ ->
                            pass

                        _ ->
                            fail "FEB supported"
            ]
        , describe "out of range"
            [ test "minutes outside range" <|
                \() ->
                    Expect.equal
                        (Err
                            [ { row = 1
                              , col = 11
                              , problem = Problem "minutes, the first part, is 61. I was expecting values in the range from 0 to 59."
                              }
                            ]
                        )
                        (Cron.fromString "61 0 0 0 0")
            , test "hours outside range" <|
                \() ->
                    Expect.equal
                        (Err
                            [ { row = 1
                              , col = 11
                              , problem = Problem "hours, the second part, is 25. I was expecting values in the range from 0 to 23."
                              }
                            ]
                        )
                        (Cron.fromString "0 25 0 0 0")
            , test "DOM outside range" <|
                \() ->
                    Expect.equal
                        (Err
                            [ { row = 1
                              , col = 11
                              , problem = Problem "day of month, the third part, is 32. I was expecting values in the range from 1 to 31."
                              }
                            ]
                        )
                        (Cron.fromString "0 0 32 0 0")
            , test "month outside range" <|
                \() ->
                    Expect.equal
                        (Err
                            [ { row = 1
                              , col = 11
                              , problem = Problem "month, the fourth part, is 13. I was expecting values in the range from 1 to 12."
                              }
                            ]
                        )
                        (Cron.fromString "0 0 1 13 0")
            , test "DOW outside range" <|
                \() ->
                    Expect.equal
                        (Err
                            [ { row = 1
                              , col = 11
                              , problem = Problem "day of week, the fifth part, is 10. I was expecting values in the range from 0 to 6."
                              }
                            ]
                        )
                        (Cron.fromString "0 0 3 1 10")
            ]
        , describe "ranges"
            [ test "out of range" <|
                \() ->
                    Expect.equal
                        (Err
                            [ { row = 1
                              , col = 16
                              , problem = Problem "day of week, the fifth part, is 7. I was expecting values in the range from 0 to 6."
                              }
                            ]
                        )
                        (Cron.fromString "* 2-5 1-2 1 7-8")
            ]
        , describe "sequences"
            [ test "out of range" <|
                \() ->
                    Expect.equal
                        (Err
                            [ { row = 1
                              , col = 19
                              , problem = Problem "day of month, the third part, is 32. I was expecting values in the range from 1 to 31."
                              }
                            ]
                        )
                        (Cron.fromString "1,2 * 30,31,32 * *")
            ]
        ]
