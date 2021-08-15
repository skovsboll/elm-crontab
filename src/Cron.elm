module Cron exposing
    ( Cron(..)
    , fromString
    , Expr(..)
    , Term(..)
    , Atom(..)
    , Month(..)
    , WeekDay(..)
    )

{-| Parses a classic UNIX style crontab string into a data structure, `Cron`, from which
you can extract information.

The main entrypoint is the fromString function.

    Cron.fromString "* * */3 4 *"

fromString returns a `Result (List DeadEnd) Cron`.

#Definition

@docs Cron


# API

@docs fromString


# Syntax Tree

@docs Expr
@docs Term
@docs Atom
@docs Month
@docs WeekDay

-}

import Parser exposing (..)


{-| A `Cron` expression consists of exactly five elements:

    1. Minutes (0-59)
    2. Hours (0-23)
    3. Day of month (1-31)
    4. Month (1-12 or jan,feb,...)
    5. Week day (0-6 or mon, tue, ...)

-}
type Cron
    = Cron (Expr Int) (Expr Int) (Expr Int) (Expr Month) (Expr WeekDay)


{-| Represents each of the five parts of a crontab

A Single is just a number or ordinal such as 1 or MON.
Multiple is a comma-separated list of terms, such as 1,2,5 or JAN,FEB
Every represents the star meaning "everything matches"

-}
type Expr a
    = Single (Term a)
    | Multiple (List (Term a))
    | Every


{-| A term can be either:

  - Step: A step such as 2/3
  - EveryStep: A star with a step, \*/4 meaning "every fourth"
  - Atom: A simple value such as 7 or MON-FRI

-}
type Term a
    = Step (Atom a) Int
    | EveryStep Int
    | Atom (Atom a)


{-| An atom is either

  - Particle: a particle or "single value" such as 9
  - Range: a range with two particles, i.e. 8-12 or FRI-SAT

-}
type Atom a
    = Particle a
    | Range a a


{-| The months of the year
-}
type Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December


{-| The week days
-}
type WeekDay
    = Sunday
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday


{-| Parse a crontab string to a Cron expression
-}
fromString : String -> Result (List DeadEnd) Cron
fromString input =
    run cron input



-----------------------------------------------
-- Cron - top level expression
-----------------------------------------------


cron : Parser Cron
cron =
    succeed Cron
        |. spaces
        |= expr (int 0 59) compare
        |. atLeastOneSpace
        |= expr (int 0 23) compare
        |. atLeastOneSpace
        |= expr (int 1 31) compare
        |. atLeastOneSpace
        |= expr month compareMonth
        |. atLeastOneSpace
        |= expr weekDay compareWeekday
        |. spaces
        |. end


compareWeekday : WeekDay -> WeekDay -> Order
compareWeekday a b =
    compare (intFromWeekDay a) (intFromWeekDay b)


compareMonth : Month -> Month -> Order
compareMonth a b =
    compare (intFromMonth a) (intFromMonth b)



-----------------------------------------------
-- Expressions
-----------------------------------------------


expr : Parser a -> (a -> a -> Order) -> Parser (Expr a)
expr particle comparer =
    succeed identity
        |= oneOf
            [ singleOrMulti particle
            , backtrackable everyExpr
            ]


everyExpr : Parser (Expr a)
everyExpr =
    map (always Every) (symbol "*")


singleOrMulti : Parser a -> Parser (Expr a)
singleOrMulti particle =
    backtrackable (multiExpr particle)
        |> andThen
            (\listTerm ->
                case listTerm of
                    [] ->
                        problem "not a list!"

                    [ head ] ->
                        succeed (Single head)

                    properList ->
                        succeed (Multiple properList)
            )


multiExpr : Parser a -> Parser (List (Term a))
multiExpr particle =
    backtrackable (loop [] (multipleTermsHelp particle))


multipleTermsHelp : Parser a -> List (Term a) -> Parser (Step (List (Term a)) (List (Term a)))
multipleTermsHelp particle revTerms =
    oneOf
        [ backtrackable
            (succeed (\term_ -> Loop (term_ :: revTerms))
                |= term particle
                |. symbol ","
            )
        , succeed (\term_ -> Done (List.reverse (term_ :: revTerms)))
            |= term particle
        , succeed () |> andThen (always (problem "not a list!"))
        ]


atLeastOneSpace : Parser ()
atLeastOneSpace =
    let
        isSpace : Char -> Bool
        isSpace =
            \c -> c == ' ' || c == '\n' || c == '\u{000D}'
    in
    succeed ()
        |. chompIf isSpace
        |. chompWhile isSpace



-----------------------------------------------
-- Terms
-----------------------------------------------


term : Parser a -> Parser (Term a)
term particle =
    oneOf
        [ backtrackable (stepTerm particle)
        , everyStepTerm
        , map Atom (atom particle)
        ]


stepTerm : Parser a -> Parser (Term a)
stepTerm particle =
    succeed Step
        |= lazy (always (atom particle))
        |. symbol "/"
        |= int 1 stepMax


stepMax : number
stepMax =
    365


everyStepTerm : Parser (Term a)
everyStepTerm =
    succeed EveryStep
        |. symbol "*"
        |. symbol "/"
        |= int 1 stepMax



-----------------------------------------------
-- Atoms
--
-- 3
-- 4-5
-- 0-1
-----------------------------------------------


atom : Parser a -> Parser (Atom a)
atom particle =
    oneOf
        [ backtrackable (rangeAtom particle)
        , singleAtom particle
        ]


singleAtom : Parser a -> Parser (Atom a)
singleAtom particle =
    map Particle particle


rangeAtom : Parser a -> Parser (Atom a)
rangeAtom particle =
    succeed Range
        |= particle
        |. symbol "-"
        |= particle



-----------------------------------------------
-- Int particle
-----------------------------------------------


int : Int -> Int -> Parser Int
int min max =
    getChompedString (chompWhile Char.isDigit)
        |> andThen
            (\str ->
                case String.toInt str of
                    Just i2 ->
                        if i2 >= min && i2 <= max then
                            succeed i2

                        else
                            problem ("The number " ++ String.fromInt i2 ++ " is unexpected. I need an integer from " ++ String.fromInt min ++ " through " ++ String.fromInt max ++ ".")

                    Nothing ->
                        problem ("I was expecting a number from " ++ String.fromInt min ++ " through " ++ String.fromInt max ++ ".")
            )



-----------------------------------------------
-- Month particle
-----------------------------------------------


month : Parser Month
month =
    oneOf
        [ monthString
        , int 1 12 |> andThen monthFromInt
        ]


monthString : Parser Month
monthString =
    chompWhile Char.isAlpha
        |> getChompedString
        |> andThen monthFromString


monthFromInt : Int -> Parser Month
monthFromInt i =
    case i of
        1 ->
            succeed January

        2 ->
            succeed February

        3 ->
            succeed March

        4 ->
            succeed April

        5 ->
            succeed May

        6 ->
            succeed June

        7 ->
            succeed July

        8 ->
            succeed August

        9 ->
            succeed September

        10 ->
            succeed October

        11 ->
            succeed November

        12 ->
            succeed December

        _ ->
            problem monthHelpMessage


intFromMonth : Month -> Int
intFromMonth m =
    case m of
        January ->
            1

        February ->
            2

        March ->
            3

        April ->
            4

        May ->
            5

        June ->
            6

        July ->
            7

        August ->
            8

        September ->
            9

        October ->
            10

        November ->
            11

        December ->
            12


monthHelpMessage : String
monthHelpMessage =
    "Expected the name of a month (jan, feb, mar etc...) or a number from 1 through 12."


monthFromString : String -> Parser Month
monthFromString string =
    case String.toLower string of
        "jan" ->
            succeed January

        "feb" ->
            succeed February

        "mar" ->
            succeed March

        "apr" ->
            succeed April

        "may" ->
            succeed May

        "jun" ->
            succeed June

        "jul" ->
            succeed July

        "aug" ->
            succeed August

        "sep" ->
            succeed September

        "oct" ->
            succeed October

        "nov" ->
            succeed November

        "dec" ->
            succeed December

        _ ->
            problem monthHelpMessage



-----------------------------------------------
-- WeekDay particle
-----------------------------------------------


weekDay : Parser WeekDay
weekDay =
    oneOf
        [ weekDayString
        , int 0 7 |> andThen weekDayFromInt
        ]


weekDayString : Parser WeekDay
weekDayString =
    chompWhile Char.isAlpha
        |> getChompedString
        |> andThen weekDayFromString


weekDayFromInt : Int -> Parser WeekDay
weekDayFromInt i =
    case i of
        0 ->
            succeed Sunday

        1 ->
            succeed Monday

        2 ->
            succeed Tuesday

        3 ->
            succeed Wednesday

        4 ->
            succeed Thursday

        5 ->
            succeed Friday

        6 ->
            succeed Saturday

        7 ->
            succeed Sunday

        _ ->
            problem weekDayHelpMessage


intFromWeekDay : WeekDay -> Int
intFromWeekDay weekday =
    case weekday of
        Sunday ->
            0

        Monday ->
            1

        Tuesday ->
            2

        Wednesday ->
            3

        Thursday ->
            4

        Friday ->
            5

        Saturday ->
            6


weekDayFromString : String -> Parser WeekDay
weekDayFromString string =
    case String.toLower string of
        "sun" ->
            succeed Sunday

        "mon" ->
            succeed Monday

        "tue" ->
            succeed Tuesday

        "wed" ->
            succeed Wednesday

        "thu" ->
            succeed Thursday

        "fri" ->
            succeed Friday

        "sat" ->
            succeed Saturday

        _ ->
            problem weekDayHelpMessage


weekDayHelpMessage : String
weekDayHelpMessage =
    "Expected the name of a week day (sun, mon, tue etc...) or a number from 0 through 6."
