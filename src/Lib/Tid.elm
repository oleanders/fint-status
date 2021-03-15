module Lib.Tid exposing (..)

import Time exposing (Month(..), millisToPosix)


tidMellomToTidspunkt : Time.Zone -> Int -> Int -> String
tidMellomToTidspunkt zone fra til =
    let
        diff =
            (fra - til) // 1000

        dager =
            diff // 24 // 60 // 60

        timer =
            (diff - (dager * 24 * 60 * 60)) // 60 // 60

        minutter =
            (diff - ((dager * 24 * 60 * 60) + (timer * 60 * 60))) // 60

        sekunder =
            diff - ((dager * 24 * 60 * 60) + (timer * 60 * 60) + (minutter * 60))
    in
    [ ( dager, "d" ), ( timer, "t" ), ( minutter, "m" ), ( sekunder, "s" ) ]
        |> List.filter (\( v, _ ) -> v /= 0)
        |> List.take 2
        |> List.map (\( v, t ) -> String.fromInt v ++ t)
        |> String.concat


millisTilDato : Time.Zone -> Int -> String
millisTilDato zone millis =
    let
        fix : Int -> String
        fix nr =
            if nr < 10 then
                "0" ++ String.fromInt nr

            else
                String.fromInt nr

        posix =
            millisToPosix millis

        day =
            Time.toDay zone posix
                |> String.fromInt

        month =
            case Time.toMonth zone posix of
                Jan ->
                    "jan"

                Feb ->
                    "feb"

                Mar ->
                    "mar"

                Apr ->
                    "apr"

                May ->
                    "mai"

                Jun ->
                    "jun"

                Jul ->
                    "jul"

                Aug ->
                    "aug"

                Sep ->
                    "sep"

                Oct ->
                    "okt"

                Nov ->
                    "nov"

                Dec ->
                    "des"

        year =
            posix
                |> Time.toYear zone
                |> String.fromInt

        hour =
            posix
                |> Time.toHour zone
                |> fix

        minute =
            posix
                |> Time.toMinute zone
                |> fix

        sekunder =
            posix
                |> Time.toSecond zone
                |> fix
    in
    day ++ ". " ++ month ++ " " ++ year ++ " kl. " ++ hour ++ ":" ++ minute ++ ":" ++ sekunder
