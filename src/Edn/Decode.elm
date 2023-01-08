module Edn.Decode exposing
    ( int, float, string, bool, char, keyword, symbol, nil, list, vector, set, setBy, ednMap, ednMapBy, tag, raw
    , andMap, andThen, atIndex, atKey, decodeEdn, decodeString, map, map2, map3, map4, map5, map6, map7, map8, oneOf, optional, try
    , succeed
    )

{-| Decode Edn values into Elm values. It mimics the API design of `elm/json` fairly closely.


# Decoders

@docs int, float, string, bool, char, keyword, symbol, nil, list, vector, set, setBy, ednMap, ednMapBy, tag, raw


# Combinators

@docs andMap, andThen, atIndex, atKey, decodeEdn, decodeString, map, map2, map3, map4, map5, map6, map7, map8, oneOf, optional, try

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Edn exposing (Edn(..))
import Edn.Parser
import Parser exposing (DeadEnd, Problem(..))
import Set exposing (Set)


formatCtx : Context -> String
formatCtx ctx =
    case ctx.path of
        Just path ->
            " - Path: " ++ String.join ", " path

        Nothing ->
            ""


showError : Edn -> Edn -> Context -> String
showError expected found ctx =
    "Expected: "
        ++ showType expected
        ++ " - Found: "
        ++ showType found
        ++ formatCtx ctx


showType : Edn -> String
showType value =
    case value of
        EdnString _ ->
            "String"

        EdnVariable _ ->
            "Variable"

        EdnKeyword _ _ ->
            "Keyword"

        EdnList _ ->
            "List"

        EdnVector _ ->
            "Vector"

        EdnMap _ ->
            "Map"

        EdnSet _ ->
            "Set"

        EdnNil ->
            "Nil"

        EdnBool _ ->
            "Bool"

        EdnInt _ ->
            "Int"

        EdnFloat _ ->
            "Float"

        EdnChar _ ->
            "Char"

        EdnTag _ _ _ ->
            "Tag"

        EdnSymbol _ ->
            "Symbol"


type alias Context =
    { path : Maybe (List String)
    }


type alias Decoder a =
    Context -> Edn -> Result String a


mergeCtx : Context -> Context -> Context
mergeCtx ctxA ctxB =
    { path = Just (Maybe.withDefault [] ctxA.path ++ Maybe.withDefault [] ctxB.path) }


withCtx : String -> Decoder a -> Decoder a
withCtx key decoder ctx edn =
    case
        decoder
            { ctx
                | path =
                    ctx.path
                        |> Maybe.map (\path -> path ++ [ key ])
                        |> Maybe.withDefault [ key ]
                        |> Just
            }
            edn
    of
        Ok value ->
            Ok value

        Err err ->
            Err err


find : (a -> Bool) -> List a -> Maybe a
find check items =
    case items of
        [] ->
            Nothing

        x :: xs ->
            if check x then
                Just x

            else
                find check xs


{-| Combinator to decode a certain key in an edn map

    import Edn exposing (Edn(..))
    import Edn.Decode exposing (atKey, int, decodeString)

    Expect.equal
        (decodeString
            (atKey (EdnKeyword (Just "foo") "bar") int)
            "{:foo/bar 1}"
        )
        (Ok 1)

These can be nested to get values in maps of maps of maps...

    import Edn exposing (Edn(..))
    import Edn.Decode exposing (atKey, int, decodeString)

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

-}
atKey : Edn -> Decoder a -> Decoder a
atKey key decoder =
    ednMap raw raw
        |> andThen
            (\rawList ->
                case find (Tuple.first >> (==) key) rawList of
                    Just rawValue ->
                        succeed <| Tuple.second rawValue

                    Nothing ->
                        fail <| "Key " ++ show key ++ " not found"
            )
        |> andThen (\v ctx _ -> decoder (mergeCtx ctx { path = Just [ show key ] }) v)


{-| Combinator to decode a certain index in an edn vector.
Note that unlike vectors, lists in edn are not indexed, so this combinator will not work on lists.

    import Edn exposing (Edn(..))
    import Edn.Decode exposing (atIndex, bool, decodeString)

    Expect.equal
        (decodeString
            (list (atIndex 1 bool))
            "[8 true nil]"
        )
        (Ok [ True ])

These can be nested to get values in vectors of vectors of vectors...

    import Edn exposing (Edn(..))
    import Edn.Decode exposing (atIndex, int, decodeString)

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

-}
atIndex : Int -> Decoder a -> Decoder a
atIndex index decoder =
    vector raw
        |> andThen
            (\array ->
                case Array.get index array of
                    Just rawValue ->
                        succeed rawValue

                    Nothing ->
                        fail <| "Index " ++ String.fromInt index ++ " out of bounds "
            )
        |> andThen (\v ctx _ -> decoder (mergeCtx ctx { path = Just [ String.fromInt index ] }) v)


{-| Lift a value into a decoder
-}
succeed : a -> Decoder a
succeed value _ _ =
    Ok value


{-| Lift a failure into a decoder
-}
fail : String -> Decoder a
fail error _ _ =
    Err error


{-| Compose a decoder with another decoder that acts on the previous result
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen fn decoder ctx edn =
    case decoder ctx edn of
        Ok value ->
            fn value ctx edn

        Err err ->
            Err err


{-| Chain decoders to apply all their results to a constructor one after the other.
Often used in conjection with "succeed" to start the chain. It is
advisable to use `map2`, `map3`, `map4`, etc for better positional error messaging.

    import Edn exposing (Edn(..))
    import Edn.Decode exposing (succeed, int, bool, decodeString)

    type alias MyRecord =
        { foo : Int
        , bar : Int
        , baz : Bool
        }

    Expect.equal
        (decodeString
            (succeed MyRecord
                |> andMap (atKey (EdnString "foo") int)
                |> andMap (atKey (EdnString "bar") int)
                |> andMap (atKey (EdnString "baz") bool)
            )
            """{"foo" 1, "bar" 2, "baz" true"}"""
        )
        (Ok ( 1, 2, True ))

-}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap decoder apDecoder ctx edn =
    case decoder ctx edn of
        Ok value ->
            map (\fn -> fn value) apDecoder ctx edn

        Err err ->
            Err err


{-| Transform a value from a decoder
-}
map : (a -> b) -> Decoder a -> Decoder b
map fn decoder ctx edn =
    Result.map
        fn
        (decoder ctx edn)


{-| The equivalent of chaining `andMap` twice following `succeed`

    map2 MyRecord
        (atKey (EdnString "foo") int)
        (atKey (EdnString "bar") int)

is equivalent to

    succeed MyRecord
        |> andMap (atKey (EdnString "foo") int)
        |> andMap (atKey (EdnString "bar") int)

-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 fn decoderA decoderB =
    map fn (withCtx "Field 1" decoderA)
        |> andMap (withCtx "Field 2" decoderB)


{-| The equivalent of chaining `andMap` three times following `succeed`

    map3 MyRecord
        (atKey (EdnString "foo") int)
        (atKey (EdnString "bar") int)
        (atKey (EdnString "baz") bool)

is equivalent to

    succeed MyRecord
        |> andMap (atKey (EdnString "foo") int)
        |> andMap (atKey (EdnString "bar") int)
        |> andMap (atKey (EdnString "baz") bool)

-}
map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 fn decoderA decoderB decoderC =
    map fn (withCtx "Field 1" decoderA)
        |> andMap (withCtx "Field 2" decoderB)
        |> andMap (withCtx "Field 3" decoderC)


{-| The equivalent of chaining `andMap` four times following `succeed`.
See the documentation of `map2` and `map3` for examples.
-}
map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map4 fn decoderA decoderB decoderC decoderD =
    map fn (withCtx "Field 1" decoderA)
        |> andMap (withCtx "Field 2" decoderB)
        |> andMap (withCtx "Field 3" decoderC)
        |> andMap (withCtx "Field 4" decoderD)


{-| The equivalent of chaining `andMap` five times following `succeed`.
See the documentation of `map2` and `map3` for examples.
-}
map5 : (a -> b -> c -> d -> e -> f) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f
map5 fn decoderA decoderB decoderC decoderD decoderE =
    map fn (withCtx "Field 1" decoderA)
        |> andMap (withCtx "Field 2" decoderB)
        |> andMap (withCtx "Field 3" decoderC)
        |> andMap (withCtx "Field 4" decoderD)
        |> andMap (withCtx "Field 5" decoderE)


{-| The equivalent of chaining `andMap` six times following `succeed`.
See the documentation of `map2` and `map3` for examples.
-}
map6 : (a -> b -> c -> d -> e -> f -> g) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g
map6 fn decoderA decoderB decoderC decoderD decoderE decoderF =
    map fn (withCtx "Field 1" decoderA)
        |> andMap (withCtx "Field 2" decoderB)
        |> andMap (withCtx "Field 3" decoderC)
        |> andMap (withCtx "Field 4" decoderD)
        |> andMap (withCtx "Field 5" decoderE)
        |> andMap (withCtx "Field 6" decoderF)


{-| The equivalent of chaining `andMap` seven times following `succeed`.
See the documentation of `map2` and `map3` for examples.
-}
map7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h
map7 fn decoderA decoderB decoderC decoderD decoderE decoderF decoderG =
    map fn (withCtx "Field 1" decoderA)
        |> andMap (withCtx "Field 2" decoderB)
        |> andMap (withCtx "Field 3" decoderC)
        |> andMap (withCtx "Field 4" decoderD)
        |> andMap (withCtx "Field 5" decoderE)
        |> andMap (withCtx "Field 6" decoderF)
        |> andMap (withCtx "Field 7" decoderG)


{-| The equivalent of chaining `andMap` eight times following `succeed`.
See the documentation of `map2` and `map3` for examples.
-}
map8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder i
map8 fn decoderA decoderB decoderC decoderD decoderE decoderF decoderG decoderH =
    map fn (withCtx "Field 1" decoderA)
        |> andMap (withCtx "Field 2" decoderB)
        |> andMap (withCtx "Field 3" decoderC)
        |> andMap (withCtx "Field 4" decoderD)
        |> andMap (withCtx "Field 5" decoderE)
        |> andMap (withCtx "Field 6" decoderF)
        |> andMap (withCtx "Field 7" decoderG)
        |> andMap (withCtx "Field 8" decoderH)


{-| apply a decoder to a string value to get a result
-}
decodeString : Decoder a -> String -> Result String a
decodeString decoder ednString =
    Edn.Parser.run ednString
        |> Result.mapError deadEndsToString
        |> Result.andThen (decodeEdn decoder)


{-| apply a decoder to a raw `Edn` value to get a result.

The `Edn` type is described in [edn-parser](https://package.elm-lang.org/packages/abradley2/edn-parser/latest/Edn)

-}
decodeEdn : Decoder a -> Edn -> Result String a
decodeEdn decoder edn =
    decoder { path = Nothing } edn


{-| A decoder that doesn't do anything, and just returns a raw `Edn` value.

The `Edn` type is described in [edn-parser](https://package.elm-lang.org/packages/abradley2/edn-parser/latest/Edn)

-}
raw : Decoder Edn
raw _ value =
    Ok value


{-| Decode an edn list and apply a decoder to each element of the list.
-}
list : Decoder a -> Decoder (List a)
list decoder ctx value =
    case value of
        EdnList listValue ->
            listHelper decoder ctx 0 listValue []

        _ ->
            Err <| showError (EdnList []) value ctx


listHelper : Decoder a -> Context -> Int -> List Edn -> List a -> Result String (List a)
listHelper decoder ctx idx values decodedValues =
    case values of
        value :: next ->
            case
                decoder
                    { ctx
                        | path =
                            Maybe.map (\path -> path ++ [ String.fromInt idx ])
                                ctx.path
                                |> Maybe.withDefault [ String.fromInt idx ]
                                |> Just
                    }
                    value
            of
                Ok decodedValue ->
                    listHelper decoder ctx (idx + 1) next (decodedValue :: decodedValues)

                Err err ->
                    Err err

        [] ->
            Ok (List.reverse decodedValues)


{-| Decode an edn set and apply a decoder to each element of the set.

This decoder will return a list of the decoded values, not a set. To get
an Elm set, use `setBy` instead, which requires you to convert the set values
into `comparable` values.

-}
set : Decoder a -> Decoder (List a)
set decoder ctx value =
    case value of
        EdnSet setValue ->
            decodeEdn (list decoder) (EdnList setValue)

        _ ->
            Err <| showError (EdnSet []) value ctx


{-| Decode an edn set and apply a decoder to each element of the set.

The second argument is a function which takes each decoded item
and returns it as a `comparable` so that it may properly populate
an Elm `Set`

-}
setBy : Decoder a -> (a -> comparable) -> Decoder (Set comparable)
setBy decoder toComparable =
    map (List.map toComparable >> Set.fromList) (set decoder)


{-| Decode an edn map and apply two decoders to each key and value of the map
respectively. Edn maps can have keys of any type so this decodes into a list
of key/value tuples.

To decode into an Elm `Dict`, use `ednMapBy` instead.

-}
ednMap : Decoder key -> Decoder value -> Decoder (List ( key, value ))
ednMap keyDecoder valueDecoder ctx value =
    case value of
        EdnMap mapValue ->
            let
                keyList =
                    decodeEdn (list keyDecoder) (EdnList (List.map Tuple.first mapValue))

                valueList =
                    decodeEdn (list valueDecoder) (EdnList (List.map Tuple.second mapValue))
            in
            Result.map2
                (List.map2 Tuple.pair)
                keyList
                valueList

        _ ->
            Err <| showError (EdnMap []) value ctx


{-| Decode an edn map and apply two decoders to each key and value of the map
respectively.

Accepts an additional argument that maps the keys to `comparable` types in order to return
an Elm `Dict`.

-}
ednMapBy : Decoder key -> Decoder value -> (key -> comparable) -> Decoder (Dict comparable value)
ednMapBy keyDecoder valueDecoder toComparable =
    map
        (List.map (\( key, value ) -> ( toComparable key, value )) >> Dict.fromList)
        (ednMap keyDecoder valueDecoder)


{-| Decode an edn vector into an Elm `Array`, applying the decoder to each item
-}
vector : Decoder a -> Decoder (Array a)
vector decoder ctx value =
    case value of
        EdnVector arrayValue ->
            decodeEdn (list decoder) (EdnList (Array.toList arrayValue))
                |> Result.map Array.fromList

        _ ->
            Err <| showError (EdnVector (Array.fromList [])) value ctx


{-| Decode an edn int into an elm `Int`
-}
int : Decoder Int
int ctx value =
    case value of
        EdnInt intValue ->
            Ok intValue

        _ ->
            Err <| showError (EdnInt 0) value ctx


{-| Decode an edn float into an elm `Float`
-}
float : Decoder Float
float ctx value =
    case value of
        EdnInt intValue ->
            Ok (toFloat intValue)

        EdnFloat floatValue ->
            Ok floatValue

        _ ->
            Err <| showError (EdnFloat 0.0) value ctx


{-| Decode an edn string into an elm `String`
-}
string : Decoder String
string ctx value =
    case value of
        EdnString stringValue ->
            Ok stringValue

        _ ->
            Err <| showError (EdnString "") value ctx


{-| Decode an edn char into an elm `Char`.

Keep in mind the way edn strings escape unicode is different than in Elm strings.

-}
char : Decoder Char
char ctx value =
    case value of
        EdnChar charValue ->
            Ok charValue

        _ ->
            Err <| showError (EdnChar 'x') value ctx


{-| Decode an edn bool into an elm `Bool`
-}
bool : Decoder Bool
bool ctx value =
    case value of
        EdnBool boolValue ->
            Ok boolValue

        _ ->
            Err <| showError (EdnBool False) value ctx


{-| Decode an edn nil into `()`. This isn't super useful except for handling optional values
in combination with `oneOf`, but you likely just want to use the `optional` combinator instead.
-}
nil : Decoder ()
nil ctx value =
    case value of
        EdnNil ->
            Ok ()

        _ ->
            Err <| showError EdnNil value ctx


{-| Decode an edn keyword into an elm `Tuple` consisting of an optional namespace and then
the keyword
-}
keyword : Decoder ( Maybe String, String )
keyword ctx value =
    case value of
        EdnKeyword ns kw ->
            Ok ( ns, kw )

        _ ->
            Err <| showError (EdnKeyword Nothing "") value ctx


{-| Symbols in edn can represent special names and operations
that are neither keywords or strings. They're generally only needed for complex situations
like DSL's

    type Operator
        = Sum
        | Prod
        | Div

    opDecoder : Decoder Operator
    opDecoder =
        oneOf
            [ symbol "+" |> map (\_ -> Sum)
            , symbol "*" |> map (\_ -> Prod)
            , symbol "/" |> map (\_ -> Div)
            ]

-}
symbol : String -> Decoder String
symbol symbolRep ctx value =
    case value of
        EdnSymbol symbolValue ->
            if symbolRep == symbolValue then
                Ok symbolValue

            else
                Err <| showError (EdnSymbol symbolRep) value ctx

        _ ->
            Err <| showError (EdnSymbol "") value ctx


{-| Decode an edn tag into an elm `Tuple` consisting of a required namespace, a tag name,
and then a nested edn value for the decoder argument
-}
tag : Decoder a -> Decoder ( String, String, a )
tag decoder ctx value =
    case value of
        EdnTag ns tg val ->
            -- TODO: add tag name to context here
            decoder ctx val
                |> Result.map (\v -> ( ns, tg, v ))

        _ ->
            Err <| showError (EdnTag "" "" EdnNil) value ctx


{-| Try a list of decoders and take the first successful value or
return the first error
-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    oneOfHelper decoders Nothing


oneOfHelper : List (Decoder a) -> Maybe String -> Decoder a
oneOfHelper decoders maybeErr ctx value =
    case decoders of
        [] ->
            case maybeErr of
                Just err ->
                    Err err

                Nothing ->
                    Err ("No Match " ++ formatCtx ctx)

        decoder :: next ->
            case decoder ctx value of
                Ok result ->
                    Ok result

                Err err ->
                    oneOfHelper next (Just err) ctx value


{-| Decode a value that may also be an edn nil into an elm `Maybe`.
This is not for decoders that may fail in any other. Use `try` for that.
-}
optional : Decoder a -> Decoder (Maybe a)
optional decoder =
    oneOf [ map Just decoder, map (always Nothing) nil ]


{-| Force a decoder to succeed by nesting the `Result` returned in an `Ok` value.
Useful for ignoring errors for particularly troublesome data without losing the
error details.
-}
try : Decoder a -> Decoder (Result String a)
try decoder ctx value =
    case decoder ctx value of
        Ok result ->
            Ok (Ok result)

        Err err ->
            Ok (Err err)


show : Edn -> String
show edn =
    case edn of
        EdnString s ->
            "\"" ++ s ++ "\""

        EdnVariable s ->
            "#" ++ s

        EdnKeyword ns s ->
            case ns of
                Just nsval ->
                    ":" ++ nsval ++ "/" ++ s

                Nothing ->
                    ":" ++ s

        EdnList l ->
            "(" ++ String.join " " (List.map show l) ++ ")"

        EdnVector a ->
            "[" ++ String.join " " (Array.toList (Array.map show a)) ++ "]"

        EdnMap l ->
            "{" ++ String.join " " (List.map (\( k, v ) -> show k ++ " " ++ show v) l) ++ "}"

        EdnSet l ->
            "#{" ++ String.join " " (List.map show l) ++ "}"

        EdnNil ->
            "nil"

        EdnBool b ->
            if b then
                "true"

            else
                "false"

        EdnInt i ->
            String.fromInt i

        EdnFloat f ->
            String.fromFloat f

        EdnChar c ->
            "\\" ++ String.fromChar c

        EdnTag ns name v ->
            "#" ++ ns ++ "/" ++ name ++ " " ++ show v

        EdnSymbol s ->
            s


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Problem -> String
problemToString p =
    case p of
        Expecting s ->
            "expecting '" ++ s ++ "'"

        ExpectingInt ->
            "expecting int"

        ExpectingHex ->
            "expecting hex"

        ExpectingOctal ->
            "expecting octal"

        ExpectingBinary ->
            "expecting binary"

        ExpectingFloat ->
            "expecting float"

        ExpectingNumber ->
            "expecting number"

        ExpectingVariable ->
            "expecting variable"

        ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        ExpectingEnd ->
            "expecting end"

        UnexpectedChar ->
            "unexpected char"

        Problem s ->
            "problem " ++ s

        BadRepeat ->
            "bad repeat"
