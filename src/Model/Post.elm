module Model.Post exposing (..)
import Json.Decode as De
import Time

type alias Post =
    { by : String
    , id : Int
    , score : Int
    , title : String
    , url : Maybe String
    , time : Time.Posix
    , type_ : String
    }

{-| Decode a `Post` -}
decode : De.Decoder Post
decode =
    De.map7 Post
        (De.field "by" De.string)
        (De.field "id" De.int)
        (De.field "score" De.int)
        (De.field "title" De.string)
        (De.field "url" (De.maybe De.string))
        (De.field "time" (De.map (Time.millisToPosix << (*) 1000) De.int))
        (De.field "type" De.string)