-module(merge_sort).
-export([sort/1]).

sort([]) -> [];  
sort([X]) -> [X]; 
sort(List) ->
    
    {Left, Right} = split(List),
    
    
    SortedLeft = sort(Left),
    SortedRight = sort(Right),
    
    
    merge(SortedLeft, SortedRight).


split([]) -> {[], []};
split([X]) -> {[X], []};
split([X, Y | Rest]) ->
    {Left, Right} = split(Rest),
    {[X | Left], [Y | Right]}.


merge([], L) -> L;
merge(L, []) -> L;
merge([X | RestX], [Y | RestY]) when X =< Y ->
    [X | merge(RestX, [Y | RestY])];
merge([X | RestX], [Y | RestY]) when X > Y ->
    [Y | merge([X | RestX], RestY)].
