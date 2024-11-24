module View.Posts exposing (..)

import Html exposing (Html, div, text, table, thead, tbody, tr, th, td, a, label, select, input)
import Html.Attributes exposing (href, class, id, value, selected, type_)
import Html.Events exposing (..)
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time
import Html.Events exposing (onInput)
import Html exposing (option)
import Html.Attributes exposing (checked)
import Cursor exposing (current)


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
postTable config currentTime posts =
  let
      processedPosts = filterPosts config posts
  in
    div []
      [ table []
          [ thead []
            [ tr[]
             [ th [] [text "Score"]
              ,th [] [text "Title"]
              ,th [] [text "Type"]
              ,th [] [text "Posted Date"]
              ,th [] [text "Link"]
             ]
            ]
          , tbody [] (List.map (postRow currentTime) processedPosts)
          ]
      ]

postRow : Time.Posix -> Post -> Html Msg
postRow currentTime post =
  let
      zone = Time.utc
      absoluteTime = Util.Time.formatTime zone post.time
      relativeDuration =
        case Util.Time.durationBetween post.time currentTime of
            Just duration -> " (" ++ Util.Time.formatDuration duration ++ ")"
            Nothing -> ""
  in
  tr[]
  [ td [class "post-score"] [text (String.fromInt post.score)]
  , td [class "post-title"] [text post.title]
  , td [class "post-type"] [text post.type_]
  , td [class "post-time"] [text (absoluteTime ++ relativeDuration)]
  , td [class "post-url"]
    [
      case post.url of
          Just url -> a [href url] [ text "Link"]
          Nothing -> text "No link"
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
postsConfigView config =
  div[]
    [
    div[][
    label [] [text "Posts per page: "]
     , select [ id "select-posts-per-page", onInput (\value ->  ConfigChanged (SetPostsToShow (Maybe.withDefault 0 (String.toInt value))))] --if the input is incompatible (cant be cast to String), 0 products will be listed
     [ option [value "10", selected (config.postsToShow == 10)] [text "10"]
     , option [value "25", selected (config.postsToShow == 25)] [text "25"]
     , option [value "50", selected (config.postsToShow == 50)] [text "50"]
     ]
     ]
     ,div[][
      label [][text "Sort by: "]
     , select [id "select-sort-by", onInput (\value ->  ConfigChanged (SetSortBy (Maybe.withDefault None (sortFromString value))))] -- if problems, sort by nothing
     [ option [value "Score", selected (config.sortBy == Score)][text "Score"]
     , option [value "Title", selected (config.sortBy == Title)][text "Title"]
     , option [value "Posted", selected (config.sortBy == Posted)][text "Posted"]
     , option [value "None", selected (config.sortBy == None)][text "None"]
     ]
     ]
     ,div[]
     [ input [type_ "checkbox", id "checkbox-show-job-posts", onCheck (\checked -> ConfigChanged (ToggleShowJobs checked)) , checked config.showJobs] [] --we use not because we want to flip the state from the previous one
     , text "Show job posts."
     ]
     ,div[]
     [
      input [type_ "checkbox", id "checkbox-show-text-only-posts", onCheck (\checked -> ConfigChanged (ToggleShowTextOnly checked)) , checked config.showTextOnly] []
     , text "Show text only posts."
     ]

    ]