module Model.PostsConfig exposing
    (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)

import Html.Attributes exposing (scope)
import Model.Post exposing (Post)
import Time


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
        Score -> "Score"
        Title -> "Title"
        Posted -> "Posted"
        None -> "None"


sortFromString : String -> Maybe SortBy
sortFromString str =
    case str of
        "Score" -> Just Score
        "Title" -> Just Title
        "Posted" -> Just Posted
        "None" -> Just None
        _ -> Nothing


sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score -> \postA postB -> compare postB.score postA.score
        Title -> \postA postB -> compare postA.title postB.title
        Posted -> \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)
        None -> \_ _ -> EQ


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
    = ChangePostsToShow Int
    | ChangeSortBy SortBy
    | ChangeShowJobs Bool
    | ChangeShowTextOnly Bool


applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges change config =
    case change of
        ChangePostsToShow postsToShow ->
            { config | postsToShow = postsToShow }

        ChangeSortBy sortBy ->
            { config | sortBy = sortBy }

        ChangeShowJobs showJobs ->
            { config | showJobs = showJobs }

        ChangeShowTextOnly showTextOnly ->
            { config | showTextOnly = showTextOnly }


filterPosts : PostsConfig -> List Post -> List Post
filterPosts config posts =
    let
        sortedPosts = List.sortWith (sortToCompareFn config.sortBy) posts
    in
    case config.postsToShow of
        10 -> List.take 10 sortedPosts
        25 -> List.take 25 sortedPosts
        50 -> List.take 50 sortedPosts
        _ -> sortedPosts
