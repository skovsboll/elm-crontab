module Cron exposing (Atom(..), Cron(..), Expr(..), Month(..), Term(..), WeekDay(..), fromString)

{-| Parses a classic UNIX style crontab string into a data structure from which
you can extract information.

The main entrypoint is the fromString function.

    Cron.fromString "* * */3 4 *"

fromString returns a Result (List DeadEnd) Cron.

A Cron expression consists of exactly five elements:

    1. Minutes
    2. Hours
    3. Day of month (1-31)
    4. Month (1-12 or jan,feb,...)
    5. Week day (0-6 or mon, tue, ...)

-}

import Parser exposing (..)


type Cron
    = Cron (Expr Int) (Expr Int) (Expr Int) (Expr Month) (Expr WeekDay)


type Expr a
    = Single (Term a)
    | Multiple (List (Term a))
    | Every


type Term a
    = Step (Atom a) Int
    | EveryStep Int
    | Simple (Atom a)


type Atom a
    = Particle a
    | Range a a


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
        |= expr (int 0 59)
        |= expr (int 0 23)
        |= expr (int 1 31)
        |= expr month
        |= expr weekDay
        |. end



-----------------------------------------------
-- Expressions
-----------------------------------------------


expr : Parser a -> Parser (Expr a)
expr particle =
    succeed identity
        |. spaces
        |= oneOf
            [ singleOrMulti particle
            , backtrackable everyExpr
            ]
        |. spaces


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



-----------------------------------------------
-- Terms
-----------------------------------------------


term : Parser a -> Parser (Term a)
term particle =
    oneOf
        [ backtrackable (stepTerm particle)
        , everyStepTerm
        , map Simple (atom particle)
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
            (\i ->
                case String.toInt i of
                    Just i2 ->
                        if i2 >= min && i2 <= max then
                            succeed i2

                        else
                            problem ("Expected an integer from " ++ String.fromInt min ++ " through " ++ String.fromInt max ++ ".")

                    Nothing ->
                        problem "not a valid int"
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
        , int 0 6 |> andThen weekDayFromInt
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

        _ ->
            problem weekDayHelpMessage


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
