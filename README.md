elm-cron
===

### A crontab parser and humanizer in Elm -- for the basic UNIX syntax only.

Inspired by [rodinalex/elm-cron](https://github.com/rodinalex/elm-cron) but rewritten from scratch using [elm/parser](https://github.com/elm/parser) and elm 0.19.1

## Installation

`elm install skovsboll/elm-cron`

Run the examples using `elm repl` :

## Parsing

```elm
import Cron exposing (fromString)

Cron.fromString " *   * *   *  *  "
-- (Ok (Cron Every Every Every Every Every))

Cron.fromString "* * */3 4 *"
-- Ok (Cron Every Every (Single (EveryStep 3)) (Single (Simple (Numeric 4))) Every)

```


## Explaining

```elm
import Cron exposing (Atom(..), Cron(..), Expr(..), Term(..))
import Humanizer

Humanizer.toString (Cron Every Every (Single (EveryStep 3)) (Single (Simple (Numeric 4))) Every)
-- "every minute, every hour, every third day of the month, in April, all week."

```


## All together now

```elm
import Cron exposing (fromString)
import Humanizer
import Parser

case Cron.fromString "30 10 * * 3" of
    Ok cron ->
        Humanizer.toString cron
    Err errors ->
        errors
            |> Parser.deadEndsToString

-- at 10:30, every day of the month, all year, on Wednesday.

```

## Detecting syntax errors

```elm

Cron.fromString "1,2 * 30,31,32 * *"
--Err
--    [ { col = 19
--      , problem = Problem "day of month, the third number, is 32. I was expecting values in the range from 1 to 31."
--      , row = 1
--      }
--    ]

```


## Where to go from here?

See the [tests](tree/main/tests)


## Caveats

Does neither support month abbreviations (JAN, FEB, MAR) nor day-of-week abbreviations (SUN, MON, ...).

No other fancy syntax supported.



## Licence 

[Free Public License 1.0.0](https://tldrlegal.com/license/free-public-license-1.0.0#summary)

TLDR; Do what you want with it, but don't hold me liable for damages.


## Contributing

Open issue
Fork
Fix
Submit PR
