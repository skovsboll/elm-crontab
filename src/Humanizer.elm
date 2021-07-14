module Humanizer exposing (toString)

import Cron exposing (Atom(..), Cron(..), Expr(..), Month(..), Term(..), WeekDay(..))


toString : Cron -> String
toString (Cron m h dm mo dw) =
    String.join ", "
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


time : Expr Int -> Expr Int -> String
time m h =
    case ( m, h ) of
        ( Single (Simple (Particle m_)), Single (Simple (Particle h_)) ) ->
            "at " ++ String.fromInt h_ ++ ":" ++ String.fromInt m_

        _ ->
            minute m ++ ", " ++ hour h



-----------------------------------------------
-- Minute
-----------------------------------------------


minute : Expr Int -> String
minute a =
    case a of
        Single term ->
            minuteTerm term

        Multiple terms ->
            List.map minuteTerm terms
                |> String.join " and "

        Every ->
            "every minute"


minuteTerm : Term Int -> String
minuteTerm term =
    case term of
        Step atom int ->
            "every " ++ ordinalFraction int ++ " minute " ++ atomAsRange atom 59

        EveryStep int ->
            "every " ++ ordinalFraction int ++ " minute"

        Simple atom ->
            minuteAtomToString atom ++ " minutes past"


minuteAtomToString : Atom Int -> String
minuteAtomToString atom =
    case atom of
        Particle a ->
            "at " ++ String.fromInt a

        Range a b ->
            "from " ++ String.fromInt a ++ " through " ++ String.fromInt b



-----------------------------------------------
-- Hour
-----------------------------------------------


hour : Expr Int -> String
hour a =
    case a of
        Single term ->
            hourTerm term

        Multiple terms ->
            List.map hourTerm terms
                |> String.join " and "

        Every ->
            "every hour"


hourTerm : Term Int -> String
hourTerm term =
    case term of
        Step atom int ->
            "every " ++ ordinalFraction int ++ " hour " ++ atomAsRange atom 23

        EveryStep int ->
            "every " ++ ordinalFraction int ++ " hour"

        Simple atom ->
            hourAtomToString atom


hourAtomToString : Atom Int -> String
hourAtomToString atom =
    case atom of
        Particle a ->
            "past " ++ String.fromInt a

        Range a b ->
            "from " ++ String.fromInt a ++ " through " ++ String.fromInt b ++ " o'clock"



-----------------------------------------------
-- Day of month
-----------------------------------------------


dom : Expr Int -> String
dom a =
    case a of
        Single term ->
            domTerm term

        Multiple terms ->
            List.map domTerm terms
                |> String.join " and "

        Every ->
            "every day of the month"


domTerm : Term Int -> String
domTerm term =
    case term of
        Step atom int ->
            "every " ++ ordinalFraction int ++ " day of the month " ++ atomAsRange atom 31

        EveryStep int ->
            "every " ++ ordinalFraction int ++ " day of the month"

        Simple atom ->
            domAtomToFractionString atom ++ " day of the month"


domAtomToFractionString : Atom Int -> String
domAtomToFractionString atom =
    case atom of
        Particle a ->
            "on the " ++ ordinalFraction a

        Range a b ->
            "from the " ++ ordinalFraction a ++ " through the " ++ ordinalFraction b



-----------------------------------------------
-- Month
-----------------------------------------------


month : Expr Month -> String
month a =
    case a of
        Single term ->
            monthTerm term

        Multiple terms ->
            List.map monthTerm terms
                |> String.join " and "

        Every ->
            "all year"


monthTerm : Term Month -> String
monthTerm term =
    case term of
        Step atom int ->
            "every " ++ ordinalFraction int ++ " month " ++ monthAtomAsRange atom December

        EveryStep int ->
            "every " ++ ordinalFraction int ++ " month"

        Simple atom ->
            monthAtom atom


monthAtom : Atom Month -> String
monthAtom atom =
    case atom of
        Particle a ->
            "in " ++ stringFromMonth a

        Range a b ->
            "from " ++ stringFromMonth a ++ " through " ++ stringFromMonth b


monthAtomAsRange : Atom Month -> Month -> String
monthAtomAsRange atom max =
    case atom of
        Particle from ->
            monthAtom (Range from max)

        Range a b ->
            monthAtom (Range a b)


stringFromMonth : Month -> String
stringFromMonth m =
    case m of
        January ->
            "January"

        February ->
            "February"

        March ->
            "March"

        April ->
            "April"

        May ->
            "May"

        June ->
            "June"

        July ->
            "July"

        August ->
            "August"

        September ->
            "September"

        October ->
            "October"

        November ->
            "November"

        December ->
            "December"



-----------------------------------------------
-- Day of the week
-----------------------------------------------


dow : Expr WeekDay -> String
dow a =
    case a of
        Single term ->
            dowTerm term

        Multiple terms ->
            List.map dowTerm terms
                |> String.join " and "

        Every ->
            "all week"


dowTerm : Term WeekDay -> String
dowTerm term =
    case term of
        Step atom int ->
            "every " ++ ordinalFraction int ++ " day of the week " ++ dowAtomAsRange atom Saturday

        EveryStep int ->
            "every " ++ ordinalFraction int ++ " day of the week"

        Simple atom ->
            dowAtom atom


dowAtom : Atom WeekDay -> String
dowAtom atom =
    case atom of
        Particle a ->
            "on " ++ ordinalDayOfWeek a

        Range a b ->
            "from " ++ ordinalDayOfWeek a ++ " through " ++ ordinalDayOfWeek b


dowAtomAsRange : Atom WeekDay -> WeekDay -> String
dowAtomAsRange atom max =
    case atom of
        Particle from ->
            dowAtom (Range from max)

        Range a b ->
            dowAtom (Range a b)


ordinalDayOfWeek : WeekDay -> String
ordinalDayOfWeek a =
    case a of
        Sunday ->
            "Sunday"

        Monday ->
            "Monday"

        Tuesday ->
            "Tuesday"

        Wednesday ->
            "Wednesday"

        Thursday ->
            "Thursday"

        Friday ->
            "Friday"

        Saturday ->
            "Saturday"



-----------------------------------------------
-- Util
-----------------------------------------------


atomToString : Atom Int -> String
atomToString atom =
    case atom of
        Particle a ->
            String.fromInt a

        Range a b ->
            "from " ++ String.fromInt a ++ " through " ++ String.fromInt b


atomAsRange : Atom Int -> Int -> String
atomAsRange atom max =
    case atom of
        Particle from ->
            atomToString (Range from max)

        Range a b ->
            atomToString (Range a b)


atomToFractionString : Atom Int -> String
atomToFractionString atom =
    case atom of
        Particle a ->
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
