module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)

import Model.Post exposing (Post)
import Time exposing (posixToMillis)
import List exposing (filter, sortWith, take)
import Maybe exposing (withDefault)

type SortBy
    = Score
    | Title
    | Posted
    | None

sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]

sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"
        Title ->
            "Title"
        Posted ->
            "Posted"
        None ->
            "None"

sortFromString : String -> Maybe SortBy
sortFromString s =
    case s of
        "Score" ->
            Just Score
        "Title" ->
            Just Title
        "Posted" ->
            Just Posted
        "None" ->
            Just None
        _ ->
            Nothing

sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score ->
            \postA postB -> compare postB.score postA.score
        Title ->
            \postA postB -> compare postA.title postB.title
        Posted ->
            \postA postB -> compare (posixToMillis postB.time) (posixToMillis postA.time)
        None ->
            \_ _ -> EQ

type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }

defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True

type Change
    = ChangeTODO

applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges _ config =
    config -- Implement as needed

filterPosts : PostsConfig -> List Post -> List Post
filterPosts config posts =
    let
        filteredPosts =
            posts
                |> filter (\post -> if config.showTextOnly then post.type_ == "text" else True)
                |> filter (\post -> if not config.showJobs then post.type_ /= "job" else True)

        sortedPosts =
            sortWith (sortToCompareFn config.sortBy) filteredPosts
    in
    take config.postsToShow sortedPosts