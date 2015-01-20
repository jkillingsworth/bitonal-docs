module BitonalDocs.Compression

//-------------------------------------------------------------------------------------------------

let compressUsingPackBits scanline =

    let maxRunLength = 128

    let (|BeginNextItemsToBePacked|BeginNextItemsToBeInline|End|) =
        function
        | head :: next1 :: next2 :: tail when head = next1 && head = next2
            -> BeginNextItemsToBePacked (head, next1 :: next2 :: tail)
        | head :: tail
            -> BeginNextItemsToBeInline (head, tail)
        | []
            -> End

    let (|PackedHasReachedRunLimit|PackedNextItemToBePacked|PackedNextItemToBeInline|PackedHasReachedInputEnd|) =
        function
        | (count, items), input when count = (1 - maxRunLength)
            -> PackedHasReachedRunLimit (count, items, input)
        | (count, items), head :: tail when head = List.head items
            -> PackedNextItemToBePacked (count, items, tail)
        | (count, items), head :: tail
            -> PackedNextItemToBeInline (count, items, head :: tail)
        | (count, items), []
            -> PackedHasReachedInputEnd (count, items, List.empty<byte>)

    let (|InlineHasReachedRunLimit|InlineNextItemToBePacked|InlineNextItemToBeInline|InlineHasReachedInputEnd|) =
        function
        | (count, items), input when count = (maxRunLength - 1)
            -> InlineHasReachedRunLimit (count, items, input)
        | (count, items), head :: next1 :: next2 :: tail when head = next1 && head = next2
            -> InlineNextItemToBePacked (count, items, head :: next1 :: next2 :: tail)
        | (count, items), head :: tail
            -> InlineNextItemToBeInline (count, items, head, tail)
        | (count, items), []
            -> InlineHasReachedInputEnd (count, items, List.empty<byte>)

    let rec loop acc = function
        | BeginNextItemsToBePacked (head, tail) -> loopPacked acc ((0, [head]), tail)
        | BeginNextItemsToBeInline (head, tail) -> loopInline acc ((0, [head]), tail)
        | End -> acc
    and loopPacked acc = function
        | PackedHasReachedRunLimit (count, items, input)
        | PackedHasReachedInputEnd (count, items, input)
        | PackedNextItemToBeInline (count, items, input) -> loop ((count, items) :: acc) input
        | PackedNextItemToBePacked (count, items, tail)
            ->
            let state = count - 1, items
            loopPacked acc (state, tail)
    and loopInline acc = function
        | InlineHasReachedRunLimit (count, items, input)
        | InlineHasReachedInputEnd (count, items, input)
        | InlineNextItemToBePacked (count, items, input) -> loop ((count, items) :: acc) input
        | InlineNextItemToBeInline (count, items, head, tail)
            ->
            let state = count + 1, head :: items
            loopInline acc (state, tail)

    scanline
    |> List.ofArray
    |> loop []
    |> List.map (fun (count, items) -> (byte count) :: (List.rev items))
    |> List.reduce (fun x y -> y @ x)
    |> List.toArray
