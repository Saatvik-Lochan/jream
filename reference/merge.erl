-module(merge).
-export([main/0, merge_sort/1]).

main() ->
  {ok, [List]} = file:consult("list.txt"),
  io:write(merge_sort(List)).

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
    split(length(List) div 2, List).

split(_, []) -> {[], []};
split(0, List) -> {[], List};
split(N, [Head | Tail]) when N > 0 ->
    {First, Second} = split(N - 1, Tail),
    {[Head | First], Second}.

% Merge two sorted lists
merge([], Right) -> Right;
merge(Left, []) -> Left;
merge([H1|T1] = Left, [H2|T2] = Right) ->
    case H1 =< H2 of
        true -> [H1 | merge(T1, Right)];
        false -> [H2 | merge(Left, T2)]
    end.
