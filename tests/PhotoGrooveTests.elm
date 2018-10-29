module PhotoGrooveTests exposing (decoderTest)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeString)
import PhotoGroove exposing (..)
import Test exposing (..)


decoderTest : Test
decoderTest =
    test "title defaults to (untitled)" <|
        \_ ->
            """{"url": "fuits.com", "size": 5}"""
                |> decodeString photoDecoder
                |> Result.map .title
                |> Expect.equal (Ok "(untitled)")
