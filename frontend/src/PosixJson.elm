module PosixJson exposing (..)

import Json.Encode exposing (..)
import Json.Decode exposing (..)
import Time exposing (..)

jsonDecPosix : Decoder Posix
jsonDecPosix = succeed (millisToPosix 0)

jsonEncPosix : Posix -> Json.Encode.Value
jsonEncPosix posixTime = Json.Encode.int (posixToMillis posixTime)
