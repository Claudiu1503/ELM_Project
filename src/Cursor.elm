module Cursor exposing (Cursor, back, current, forward, fromList, length, nonEmpty, toList, withSelectedElement)

{-| Data structure to efficiently navigate a list forward or backward.

It stores a non-empty list as two lists and one element that is currently "selected".

For example, the list `[1, 2, 3]`, when focused on the first element, would be stored as `Cursor [] 1 [2, 3]`.
To focus on the second element, the representation becomes `Cursor [1] 2 [3]`.
Finally, focusing on the third element is: `Cursor [2, 1] 3 []`.

**Note that the left part of the list is stored in reverse order!**
-}

type Cursor a
    = Cursor (List a) a (List a)

{-| Creates a `Cursor` from the given left, current (mid), and right lists.
    The `left` list is reversed to maintain the structure where the left
    part is stored in reverse order.

    Example:
        withSelectedElement [2, 1] 3 [4, 5] --> Cursor [1, 2] 3 [4, 5]
-}
withSelectedElement : List a -> a -> List a -> Cursor a
withSelectedElement left mid right =
    Cursor (List.reverse left) mid right

{-| Creates a `Cursor` from an initial element and a list of subsequent elements.
    The `left` part is initially empty.

    Example:
        nonEmpty 1 [2, 3] --> Cursor [] 1 [2, 3]
-}
nonEmpty : a -> List a -> Cursor a
nonEmpty x xs =
    Cursor [] x xs

{-| Converts a list into a `Cursor`. Returns `Nothing` if the list is empty.

    Examples:
        fromList [1, 2, 3] --> Just (Cursor [] 1 [2, 3])
        fromList [] --> Nothing
-}
fromList : List a -> Maybe (Cursor a)
fromList list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just (Cursor [] x xs)

{-| Converts a `Cursor` back into a list.

    Example:
        toList (Cursor [2, 1] 3 [4, 5]) --> [1, 2, 3, 4, 5]
-}
toList : Cursor a -> List a
toList (Cursor left mid right) =
    (List.reverse left) ++ (mid :: right)

{-| Retrieves the currently selected element from the `Cursor`.

    Example:
        current (Cursor [1, 2] 3 [4, 5]) --> 3
-}
current : Cursor a -> a
current (Cursor _ a _) =
    a

{-| Moves the cursor forward by one position. Returns `Nothing` if the cursor is
    already at the end of the list.

    Examples:
        forward (Cursor [] 1 [2, 3]) --> Just (Cursor [1] 2 [3])
        forward (Cursor [1, 2] 3 []) --> Nothing
-}
forward : Cursor a -> Maybe (Cursor a)
forward cursor =
    case cursor of
        Cursor left mid [] ->
            Nothing

        Cursor left mid (r :: rs) ->
            Just (Cursor (mid :: left) r rs)

{-| Moves the cursor backward by one position. Returns `Nothing` if the cursor
    is already at the beginning of the list.

    Examples:
        back (Cursor [] 1 [2, 3]) --> Nothing
        back (Cursor [1] 2 [3]) --> Just (Cursor [] 1 [2, 3])
-}
back : Cursor a -> Maybe (Cursor a)
back cursor =
    case cursor of
        Cursor [] mid right ->
            Nothing

        Cursor (l :: ls) mid right ->
            Just (Cursor ls l (mid :: right))

{-| Calculates the total number of elements in the `Cursor`.

    Examples:
        length (Cursor [] 1 []) --> 1
        length (Cursor [2, 1] 3 [4, 5]) --> 5
-}
length : Cursor a -> Int
length (Cursor left _ right) =
    List.length left + 1 + List.length right
