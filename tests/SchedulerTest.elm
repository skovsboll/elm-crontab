module SchedulerTest exposing (..)

import Cron exposing (Atom(..), Cron(..), Expr(..), Month(..), Term(..), WeekDay(..))
import Expect
import Scheduler exposing (next)
import Test exposing (Test, describe, test)
import Time exposing (Month(..), utc)
import Time.Extra as Time exposing (Interval(..))


start =
    Time.Parts 2020 Jan 1 12 0 0 0
        |> Time.partsToPosix utc


sunshine : Test
sunshine =
    describe "sunshine"
        [ describe "numerical expressions"
            [ test "stars" <|
                \() ->
                    let
                        cron =
                            Cron Every Every Every Every (Single (EveryStep 3))
                    in
                    Expect.equal (Scheduler.next start cron) start
            ]
        ]
