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

-- Filter posts based on current configuration
filterPosts : PostsConfig -> List Post -> List Post
filterPosts config posts =
    let
        -- Filter for text-only posts if the config specifies so
        filteredTextOnlyPosts =
            if config.showTextOnly then
                posts
            else
                List.filter (\post -> post.url /= Nothing) posts

        -- Filter for job posts if the config specifies so
        filteredJobPosts =
            if config.showJobs then
                filteredTextOnlyPosts
            else
                List.filter (\post -> post.type_ /= "job") filteredTextOnlyPosts

        -- Sort the posts based on the specified sortBy criterion
        sortedPosts =
            List.sortWith (sortToCompareFn config.sortBy) filteredJobPosts
    in
    -- Limit the number of posts to display
    List.take config.postsToShow sortedPosts