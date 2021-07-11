module Humanizer exposing (toString)

import Cron exposing (Cron(..), Expr(..))


toString : Cron -> String
toString (Cron m h dm mo dw) =
    "At "
        ++ String.join ", "
            [ minute m
            , hour h
            , dom dm
            , month mo
            , dow dw
            ]
        ++ "."


minute : Expr -> String
minute a =
    "every minute"


hour : Expr -> String
hour a =
    "every hour"


dom : Expr -> String
dom a =
    "every day of month"


month : Expr -> String
month a =
    "every month"


dow : Expr -> String
dow a =
    "all week"
