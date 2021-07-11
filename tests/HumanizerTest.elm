module HumanizerTest exposing (..)

import Cron exposing (Atom(..), Cron(..), Expr(..), Term(..))
import Expect
import Humanizer
import Test exposing (Test, describe, test)


sunshine : Test
sunshine =
    describe "sunshine"
        [ describe "numerical expressions"
            [ test "stars" <|
                \() ->
                    Expect.equal
                        "At every minute, every hour, every day of month, every month, all week."
                        (Humanizer.toString
                            (Cron Every Every Every Every Every)
                        )
            , test "all numeric" <|
                \() ->
                    Expect.equal
                        "At every minute, every hour, every day of month, every month, on Mondays."
                        (Humanizer.toString
                            (Cron Every Every Every Every (Single (Simple (Literal 1))))
                        )
            ]
        ]
