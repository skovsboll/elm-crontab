module Scheduler exposing (..)

import Cron exposing (..)
import Time exposing (Month(..), Posix, utc)
import Time.Extra as Time exposing (Interval(..))


next : Posix -> Cron -> Posix
next start cron =
    Time.Parts 2020 Jan 1 12 0 0 0 |> Time.partsToPosix utc
