module DecodeTest exposing (..)

import Array
import Dict
import Edn exposing (Edn(..))
import Edn.Decode exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Decoding"
        [ test "tag" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (decodeString (tag int) "#app/foo 1") (Ok ( "app", "foo", 1 ))
                    , \_ -> expectError (decodeString (tag int) "#app/bar []")
                    ]
                    ()
        , test "tagWith" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (decodeString (tagWith int ( "foo", "bar" )) "#foo/bar 1") (Ok 1)
                    ]
                    ()
        , test "tagTransform" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        Expect.equal
                            (decodeString
                                (tagTransform
                                    (map2 Tuple.pair (atIndex 0 int) (atIndex 1 int))
                                    ( "math", "add" )
                                    (\( a, b ) -> Ok <| a + b)
                                )
                                "#math/add [1 2]"
                            )
                            (Ok 3)
                    ]
                    ()
        , test "int" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (decodeString int "1") (Ok 1)
                    , \_ -> Expect.equal (decodeString int "-1") (Ok -1)
                    , \_ -> expectError (decodeString int "1.0")
                    , \_ -> expectError (decodeString int "1.1")
                    , \_ -> Expect.equal (decodeString int "0") (Ok 0)
                    , \_ -> expectError (decodeString int "0.0")
                    , \_ -> Expect.equal (decodeString int "999") (Ok 999)
                    ]
                    ()
        , test "float" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (decodeString float "1") (Ok 1.0)
                    , \_ -> Expect.equal (decodeString float "-1") (Ok -1.0)
                    , \_ -> Expect.equal (decodeString float "1.0") (Ok 1.0)
                    , \_ -> Expect.equal (decodeString float "1.1") (Ok 1.1)
                    , \_ -> Expect.equal (decodeString float "0") (Ok 0.0)
                    , \_ -> Expect.equal (decodeString float "0.0") (Ok 0.0)
                    , \_ -> Expect.equal (decodeString float "999") (Ok 999.0)
                    ]
                    ()
        , test "string" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (decodeString string "\"foo\"") (Ok "foo")
                    , \_ -> Expect.equal (decodeString string "\"\"") (Ok "")
                    , \_ -> Expect.equal (decodeString string "\"\\\"\"") (Ok "\"")
                    , \_ -> Expect.equal (decodeString string "\"\\\\\"") (Ok "\\")
                    , \_ -> Expect.equal (decodeString string "\"\\n\"") (Ok "\n")
                    , \_ -> Expect.equal (decodeString string "\"\\r\"") (Ok "\u{000D}")
                    , \_ -> Expect.equal (decodeString string "\"\\t\"") (Ok "\t")
                    , \_ -> Expect.equal (decodeString string "\"\\u00a9\"") (Ok "©")
                    ]
                    ()
        , test "keyword" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (decodeString keyword ":foo") (Ok ( Nothing, "foo" ))
                    , \_ -> Expect.equal (decodeString keyword ":foo/bar") (Ok ( Just "foo", "bar" ))
                    , \_ -> expectError (decodeString keyword ":foo/bar/baz")
                    ]
                    ()
        , test "bool" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (decodeString bool "true") (Ok True)
                    , \_ -> Expect.equal (decodeString bool "false") (Ok False)
                    , \_ -> expectError (decodeString bool "nil")
                    ]
                    ()
        , test "nil" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (decodeString nil "nil") (Ok ())
                    , \_ -> expectError (decodeString nil "true")
                    ]
                    ()
        , test "list" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (decodeString (list int) "(1 2 3)") (Ok [ 1, 2, 3 ])
                    , \_ -> Expect.equal (decodeString (list raw) "()") (Ok [])
                    , \_ -> expectError (decodeString (list int) "(1 2 3.0)")
                    , \_ -> Expect.equal (decodeString (list float) "(1 2 3.1)") (Ok [ 1.0, 2.0, 3.1 ])
                    ]
                    ()
        , test "vector" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (decodeString (vector int) "[1 2 3]") (Ok (Array.fromList [ 1, 2, 3 ]))
                    , \_ -> Expect.equal (decodeString (vector raw) "[]") (Ok Array.empty)
                    , \_ -> expectError (decodeString (vector int) "[1 2 3.0]")
                    , \_ -> Expect.equal (decodeString (vector float) "[1 2 3.1]") (Ok (Array.fromList [ 1.0, 2.0, 3.1 ]))
                    ]
                    ()
        , test "raw" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (decodeString raw "1") (Ok (EdnInt 1))
                    , \_ -> Expect.equal (decodeString raw "1.0") (Ok (EdnFloat 1.0))
                    , \_ -> Expect.equal (decodeString raw "\"foo\"") (Ok (EdnString "foo"))
                    , \_ -> Expect.equal (decodeString raw ":foo") (Ok (EdnKeyword Nothing "foo"))
                    , \_ -> Expect.equal (decodeString raw "foo") (Ok (EdnSymbol "foo"))
                    ]
                    ()
        , test "map" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (decodeString (ednMap keyword int) "{:a 1 :b 2}") (Ok [ ( ( Nothing, "a" ), 1 ), ( ( Nothing, "b" ), 2 ) ])
                    , \_ -> Expect.equal (decodeString (ednMap keyword int) "{}") (Ok [])
                    ]
                    ()
        , test "mapBy" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (decodeString (ednMapBy string int identity) """{"one" 1 "two" 2}""") (Ok (Dict.fromList [ ( "one", 1 ), ( "two", 2 ) ]))
                    , \_ -> Expect.equal (decodeString (ednMapBy string int identity) """{"one" 1, "two" 2}""") (Ok (Dict.fromList [ ( "one", 1 ), ( "two", 2 ) ]))
                    ]
                    ()
        , test "atKey" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        Expect.equal
                            (decodeString
                                (list (atKey (EdnKeyword Nothing "a") int))
                                "({:a 1} {:a 2})"
                            )
                            (Ok [ 1, 2 ])
                    , \_ ->
                        Expect.equal
                            (decodeString
                                (atKey (EdnString "hello") string)
                                """{"hello" "world"}"""
                            )
                            (Ok "world")
                    , \_ ->
                        Expect.equal
                            (decodeString
                                (atKey (EdnString "foo") <|
                                    atKey (EdnString "bar") <|
                                        atKey (EdnString "baz") <|
                                            int
                                )
                                """{"foo" {"bar" {"baz" 1}}}"""
                            )
                            (Ok 1)
                    ]
                    ()
        , test "atIndex" <|
            \_ ->
                Expect.all
                    [ \fixture ->
                        Expect.equal
                            (decodeString
                                (atIndex 0 bool)
                                fixture
                            )
                            (Ok True)
                    , \fixture ->
                        Expect.equal
                            (decodeString
                                (atIndex 1 int)
                                fixture
                            )
                            (Ok 9)
                    , \fixture ->
                        Expect.equal
                            (decodeString
                                (atIndex 2 string)
                                fixture
                            )
                            (Ok "foo")
                    , \fixture ->
                        expectError
                            (decodeString
                                (atIndex 100 raw)
                                fixture
                            )
                    , \_ ->
                        Expect.equal
                            (decodeString
                                (atIndex 0 <|
                                    atIndex 0 <|
                                        atIndex 0 <|
                                            int
                                )
                                """[[[1]]]"""
                            )
                            (Ok 1)
                    ]
                    """[true 9 "foo" ]"""
        , test "nested at-selectors" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        Expect.equal
                            (decodeString
                                (atKey (EdnString "foo") <|
                                    atIndex 1 <|
                                        atKey (EdnString "biz") <|
                                            atIndex 0 <|
                                                string
                                )
                                """{"foo" ["bar" {"biz" ["baz"]}]}"""
                            )
                            (Ok "baz")
                    ]
                    ()
        , test "andMap" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        Expect.equal
                            (decodeString
                                (succeed Tuple.pair
                                    |> andMap (atKey (EdnKeyword Nothing "foo") int)
                                    |> andMap (atKey (EdnKeyword Nothing "bar") int)
                                )
                                """
                                {
                                    :foo 1
                                    :bar 2
                                }
                                """
                            )
                            (Ok ( 1, 2 ))
                    ]
                    ()
        , test "symbols" <|
            \_ ->
                let
                    opDecoder =
                        oneOf
                            [ map (\_ -> 1) (symbol "+")
                            , map (\_ -> 2) (symbol "*")
                            , map (\_ -> 3) (symbol "/")
                            ]
                in
                Expect.all
                    [ \fixture ->
                        Expect.equal
                            (decodeString
                                (atKey (EdnString "sum") opDecoder)
                                fixture
                            )
                            (Ok 1)
                    , \fixture ->
                        Expect.equal
                            (decodeString
                                (atKey (EdnString "prod") opDecoder)
                                fixture
                            )
                            (Ok 2)
                    ]
                    """
                    {
                        "sum" +
                        "prod" *
                    }
                    """
        ]


expectError : Result error value -> Expectation
expectError r =
    case r of
        Err _ ->
            Expect.pass

        _ ->
            Expect.fail "Expected error, got OK"
