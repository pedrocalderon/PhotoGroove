module PhotoGrooveTests exposing (decoderTest, stateTransitions)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (..)
import Test exposing (..)


decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (untitled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue photoDecoder
                |> Result.map .title
                |> Expect.equal (Ok "(untitled)")


clickedPhotoSelectsPhoto : Test
clickedPhotoSelectsPhoto =
    fuzz string " ClickedPhoto selects the given photo by URL" <|
        \url ->
            PhotoGroove.initialModel
                |> PhotoGroove.update (ClickedPhoto url)
                |> Tuple.first
                |> .selectedUrl
                |> Expect.equal (Just url)


loadPhotosSelectsFirstPhoto : Test
loadPhotosSelectsFirstPhoto =
    fuzz (list string) "LoadPhotos selects the first photo" <|
        \urls ->
            let
                photos =
                    List.map photoFromUrl urls
            in
            PhotoGroove.initialModel
                |> PhotoGroove.update (LoadPhotos (Ok photos))
                |> Tuple.first
                |> .selectedUrl
                |> Expect.equal (List.head urls)


stateTransitions : Test
stateTransitions =
    describe "state transitions"
        [ clickedPhotoSelectsPhoto
        , loadPhotosSelectsFirstPhoto
        ]


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }
