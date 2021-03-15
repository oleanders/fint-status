module Tests exposing (..)

import Expect
import Lib.Tid exposing (millisTilDato, tidMellomToTidspunkt)
import Test exposing (..)
import Time



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        , test "Millis til dato" <|
            \_ ->
                Expect.equal "5. mar 2021 kl. 20:52:05" (millisTilDato Time.utc 1614977525658)
        , test "Skal vise hvor lang tid det er mellom to tidspunkt" <|
            \_ ->
                Expect.equal "5t24m" (tidMellomToTidspunkt Time.utc 1614977525658 1614958033216)
        , test "Skal vise hvor lang tid det er mellom to tidspunktd" <|
            \_ ->
                Expect.equal "5t24m" (tidMellomToTidspunkt Time.utc 1614977525658 1614958033216)
        , test "Skal vise hvor lang tid det er mellom to tidspunktd 2" <|
            \_ ->
                tidMellomToTidspunkt Time.utc (60 * 1000) 0
                    |> Expect.equal "1m"
        ]
