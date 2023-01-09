# Elm Edn Decode

This library provides decoders for the `edn` type exposed by 
[edn-parser](https://github.com/abradley2/edn-parser)

The API quite closely follows the API of the standard `elm/json` library,
with a few noticeable changes due to the very different natures of `edn` and `json`

You will likely want to use this library in conjunction with 
[edn-parser](https://github.com/abradley2/edn-parser) as the `Edn` type constructor
is required to query fields in edn maps.

```elm
import Edn exposing (..)
import Edn.Decode exposing (..)

type alias Person =
    { name : String
    , age : Int
    }

personDecoder = 
    decodeString
        (map2
            Person
            (keyAt (EdnString "name") string)
            (keyAt (EdnString "age") int)
        )

rawEdn = """
    { "name" "Tony"
    , "age" 32
    }
"""

result = decodeString personDecoder rawEdn
```