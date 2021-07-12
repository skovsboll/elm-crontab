module Humanizer exposing (toString)

import Cron exposing (Atom(..), Cron(..), Expr(..), Term(..))


toString : Cron -> String
toString (Cron m h dm mo dw) =
    "At "
        ++ String.join ", "
            [ time m h
            , dom dm
            , month mo
            , dow dw
            ]
        ++ "."



-----------------------------------------------
-- Combined minute and hour
-- Special cases
-----------------------------------------------


time : Expr -> Expr -> String
time m h =
    case ( m, h ) of
        ( Single (Simple (Numeric m_)), Single (Simple (Numeric h_)) ) ->
            String.fromInt h_ ++ ":" ++ String.fromInt m_

        _ ->
            minute m ++ ", " ++ hour h



-----------------------------------------------
-- Minute
-----------------------------------------------


minute : Expr -> String
minute a =
    case a of
        Single term ->
            minuteTerm term

        Multiple terms ->
            List.map minuteTerm terms
                |> String.join " and "

        Every ->
            "every minute"


minuteTerm : Term -> String
minuteTerm term =
    case term of
        Step atom int ->
            "every " ++ ordinalFraction int ++ " minute " ++ atomAsRange atom 59

        EveryStep int ->
            "every " ++ ordinalFraction int ++ " minute"

        Simple atom ->
            atomToString atom ++ " past"



-----------------------------------------------
-- Hour
-----------------------------------------------


hour : Expr -> String
hour a =
    case a of
        Single term ->
            hourTerm term

        Multiple terms ->
            List.map hourTerm terms
                |> String.join " and "

        Every ->
            "every hour"


hourTerm : Term -> String
hourTerm term =
    case term of
        Step atom int ->
            "every " ++ ordinalFraction int ++ " hour " ++ atomAsRange atom 23

        EveryStep int ->
            "every " ++ ordinalFraction int ++ " hour"

        Simple atom ->
            "past " ++ atomToString atom



-----------------------------------------------
-- Day of month
-----------------------------------------------


dom : Expr -> String
dom a =
    case a of
        Single term ->
            domTerm term

        Multiple terms ->
            List.map domTerm terms
                |> String.join " and "

        Every ->
            "every day of the month"


domTerm : Term -> String
domTerm term =
    case term of
        Step atom int ->
            "every " ++ ordinalFraction int ++ " day of the month " ++ atomAsRange atom 31

        EveryStep int ->
            "every " ++ ordinalFraction int ++ " day of the month"

        Simple atom ->
            "on the " ++ atomToFractionString atom ++ " day of the month"



-----------------------------------------------
-- Month
-----------------------------------------------


month : Expr -> String
month a =
    case a of
        Single term ->
            monthTerm term

        Multiple terms ->
            List.map monthTerm terms
                |> String.join " and "

        Every ->
            "all year"


monthTerm : Term -> String
monthTerm term =
    case term of
        Step atom int ->
            "every " ++ ordinalFraction int ++ " month " ++ monthAtomAsRange atom 12

        EveryStep int ->
            "every " ++ ordinalFraction int ++ " month"

        Simple atom ->
            monthAtom atom


monthAtom : Atom -> String
monthAtom atom =
    case atom of
        Numeric a ->
            "in " ++ ordinalMonth a

        Range a b ->
            "from " ++ ordinalMonth a ++ " through " ++ ordinalMonth b


monthAtomAsRange : Atom -> Int -> String
monthAtomAsRange atom max =
    case atom of
        Numeric from ->
            monthAtom (Range from max)

        Range a b ->
            monthAtom (Range a b)


ordinalMonth : Int -> String
ordinalMonth m =
    case m of
        1 ->
            "January"

        2 ->
            "February"

        3 ->
            "March"

        4 ->
            "April"

        5 ->
            "May"

        6 ->
            "June"

        7 ->
            "July"

        8 ->
            "August"

        9 ->
            "September"

        10 ->
            "October"

        11 ->
            "November"

        12 ->
            "December"

        _ ->
            "Not supported"



-----------------------------------------------
-- Day of the week
-----------------------------------------------


dow : Expr -> String
dow a =
    case a of
        Single term ->
            dowTerm term

        Multiple terms ->
            List.map hourTerm terms
                |> String.join " and "

        Every ->
            "all week"


dowTerm : Term -> String
dowTerm term =
    case term of
        Step atom int ->
            "every " ++ ordinalFraction int ++ " day of the week " ++ dowAtomAsRange atom 6

        EveryStep int ->
            "every " ++ ordinalFraction int ++ " day of the week"

        Simple atom ->
            dowAtom atom


dowAtom : Atom -> String
dowAtom atom =
    case atom of
        Numeric a ->
            "on " ++ ordinalDayOfWeek a

        Range a b ->
            "from " ++ ordinalDayOfWeek a ++ " through " ++ ordinalDayOfWeek b


dowAtomAsRange : Atom -> Int -> String
dowAtomAsRange atom max =
    case atom of
        Numeric from ->
            dowAtom (Range from max)

        Range a b ->
            dowAtom (Range a b)


ordinalDayOfWeek : Int -> String
ordinalDayOfWeek a =
    case a of
        0 ->
            "Sunday"

        1 ->
            "Monday"

        2 ->
            "Tuesday"

        3 ->
            "Wednesday"

        4 ->
            "Thursday"

        5 ->
            "Friday"

        6 ->
            "Saturday"

        _ ->
            "Not supported"



-----------------------------------------------
-- Util
-----------------------------------------------


atomToString : Atom -> String
atomToString atom =
    case atom of
        Numeric a ->
            String.fromInt a

        Range a b ->
            "from " ++ String.fromInt a ++ " through " ++ String.fromInt b


atomAsRange : Atom -> Int -> String
atomAsRange atom max =
    case atom of
        Numeric from ->
            atomToString (Range from max)

        Range a b ->
            atomToString (Range a b)


atomToFractionString : Atom -> String
atomToFractionString atom =
    case atom of
        Numeric a ->
            ordinalFraction a

        Range a b ->
            "from the " ++ ordinalFraction a ++ " through the " ++ ordinalFraction b


ordinalFraction : Int -> String
ordinalFraction int =
    case int of
        1 ->
            ""

        2 ->
            "second"

        3 ->
            "third"

        4 ->
            "fourth"

        5 ->
            "fifth"

        6 ->
            "sixth"

        7 ->
            "seventh"

        8 ->
            "eight"

        9 ->
            "ninth"

        10 ->
            "tenth"

        a ->
            String.fromInt a ++ "."
