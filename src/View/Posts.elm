module View.Posts exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (href)
import Html.Events
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable _ _ posts =
    Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ text "Score" ]
                , Html.th [] [ text "Title" ]
                , Html.th [] [ text "Type" ]
                , Html.th [] [ text "Posted Date" ]
                , Html.th [] [ text "Link" ]
                ]
            ]
        , Html.tbody []
            (List.map postRow posts)
        ]


postRow : Post -> Html Msg
postRow post =
    Html.tr []
        [ Html.td [ Html.Attributes.class "post-score" ] [ text (String.fromInt post.score) ]
        , Html.td [ Html.Attributes.class "post-title" ] [ text post.title ]
        , Html.td [ Html.Attributes.class "post-type" ] [ text post.type_ ]
        , Html.td [ Html.Attributes.class "post-time" ] [ text (Util.Time.formatTime Time.utc post.time) ]
        , Html.td [ Html.Attributes.class "post-url" ]
            [ case post.url of
                Just url ->
                    Html.a [ href url ] [ text "Link" ]
                Nothing ->
                    text "No URL"
            ]
        ]




{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
postsConfigView : PostsConfig -> Html Msg
postsConfigView _ =
    -- div [] []
    Debug.todo "postsConfigView"
