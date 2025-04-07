-module(merge).
-export([merge_sort/1]).

% Entry point
merge_sort(List) ->
    case length(List) =< 1 of
        true -> List;
        false ->
            {Left, Right} = split(List),
            Parent = self(),
            spawn(fun() -> Parent ! merge_sort(Left) end),
            SortedRight = merge_sort(Right),
            receive
                SortedLeft -> merge(SortedLeft, SortedRight)
            end
    end.

% Split the list into two halves
split(List) ->
    lists:split(length(List) div 2, List).

% Merge two sorted lists
merge([], Right) -> Right;
merge(Left, []) -> Left;
merge([H1|T1] = Left, [H2|T2] = Right) ->
    case H1 =< H2 of
        true -> [H1 | merge(T1, Right)];
        false -> [H2 | merge(Left, T2)]
    end.
