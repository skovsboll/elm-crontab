module Cron exposing (Atom(..), Cron(..), Expr(..), Term(..), fromString)

import Parser exposing (..)


type Cron
    = Cron Expr Expr Expr Expr Expr


type Expr
    = Single Term
    | Multiple (List Term)
    | Every


type Term
    = Step Atom Int
    | EveryStep Int
    | Simple Atom


type Atom
    = Numeric Int
    | Range Int Int


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
        |= expr
        |= expr
        |= expr
        |= expr
        |= expr
        |. end
        |> andThen checkCron



-----------------------------------------------
-- Expressions
-----------------------------------------------


expr : Parser Expr
expr =
    succeed identity
        |. spaces
        |= oneOf
            [ singleOrMulti
            , backtrackable everyExpr
            ]
        |. spaces


everyExpr : Parser Expr
everyExpr =
    map (always Every) (symbol "*")


singleOrMulti : Parser Expr
singleOrMulti =
    backtrackable multiExpr
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


multiExpr : Parser (List Term)
multiExpr =
    backtrackable (loop [] multipleTermsHelp)


multipleTermsHelp : List Term -> Parser (Step (List Term) (List Term))
multipleTermsHelp revTerms =
    oneOf
        [ backtrackable
            (succeed (\term_ -> Loop (term_ :: revTerms))
                |= term
                |. symbol ","
            )
        , succeed (\term_ -> Done (List.reverse (term_ :: revTerms)))
            |= term
        , succeed () |> andThen (always (problem "not a list!"))
        ]



-----------------------------------------------
-- Terms
-----------------------------------------------


term : Parser Term
term =
    oneOf
        [ backtrackable stepTerm
        , everyStepTerm
        , map Simple atom
        ]
        |> andThen checkStep


stepTerm : Parser Term
stepTerm =
    succeed Step
        |= lazy (always atom)
        |. symbol "/"
        |= int


everyStepTerm : Parser Term
everyStepTerm =
    succeed EveryStep
        |. symbol "*"
        |. symbol "/"
        |= int


checkStep : Term -> Parser Term
checkStep term_ =
    let
        checkHelper : Int -> Parser Term
        checkHelper a =
            if a < 1 then
                problem ("A step value of " ++ String.fromInt a ++ " was seen. Step values must be 1 or higher.")

            else
                succeed term_
    in
    case term_ of
        Step _ a ->
            checkHelper a

        EveryStep a ->
            checkHelper a

        Simple atom_ ->
            succeed term_



-----------------------------------------------
-- Atoms
--
-- 3
-- 4-5
-- 0-1
-----------------------------------------------


atom : Parser Atom
atom =
    oneOf
        [ backtrackable rangeAtom
        , numericAtom
        ]


numericAtom : Parser Atom
numericAtom =
    map Numeric int


rangeAtom : Parser Atom
rangeAtom =
    succeed Range
        |= int
        |. symbol "-"
        |= int



-----------------------------------------------
-- Integers with leading zeroes
-----------------------------------------------


int : Parser Int
int =
    getChompedString (chompWhile Char.isDigit)
        |> andThen
            (\i ->
                case String.toInt i of
                    Just i2 ->
                        succeed i2

                    Nothing ->
                        problem "not a valid int"
            )



-----------------------------------------------
-- Range checks
-----------------------------------------------


checkCron : Cron -> Parser Cron
checkCron (Cron minute hour dom month dow) =
    checkExpr 0 59 "minutes, the first number, " minute
        |> andThen (always (checkExpr 0 23 "hours, the second number, " hour))
        |> andThen (always (checkExpr 1 31 "day of month, the third number, " dom))
        |> andThen (always (checkExpr 1 12 "month, the fourth number, " month))
        |> andThen (always (checkExpr 0 6 "day of week, the fifth number, " dow))
        |> map (\_ -> Cron minute hour dom month dow)


checkExpr : Int -> Int -> String -> Expr -> Parser Expr
checkExpr min max descriptor expr_ =
    case expr_ of
        Single v ->
            checkTerm min max descriptor v
                |> map (always (Single v))

        Multiple values ->
            case values of
                h :: rest ->
                    List.foldl (\val acc -> andThen (\_ -> checkTerm min max descriptor val) acc) (checkTerm min max descriptor h) rest
                        |> map (always (Multiple values))

                [] ->
                    problem "empty list"

        Every ->
            succeed expr_


checkTerm : Int -> Int -> String -> Term -> Parser Term
checkTerm min max descriptor term_ =
    case term_ of
        Step val step ->
            checkInt min max descriptor step
                |> andThen (always (checkAtom min max descriptor val))
                |> map (always (Step val step))

        EveryStep i ->
            checkInt 1 1000 descriptor i
                |> map (always EveryStep i)

        Simple atom_ ->
            checkAtom min max descriptor atom_
                |> map (always Simple atom_)


checkAtom : Int -> Int -> String -> Atom -> Parser Atom
checkAtom min max descriptor value_ =
    case value_ of
        Numeric i ->
            checkInt min max descriptor i
                |> map (always (Numeric i))

        Range a b ->
            checkInt min max descriptor a
                |> andThen (always (checkInt min max descriptor b))
                |> map (always (Range a b))


checkInt : Int -> Int -> String -> Int -> Parser Int
checkInt min max descriptor i =
    if min > i || max < i then
        problem (descriptor ++ "is " ++ String.fromInt i ++ ". I was expecting values in the range from " ++ String.fromInt min ++ " to " ++ String.fromInt max ++ ".")

    else
        succeed i
