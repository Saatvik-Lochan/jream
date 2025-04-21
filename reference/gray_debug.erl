-module(gray_debug).
-export([main/0]).

main() ->
  io:write(gray_burn(0, 0, 1000)).

gray(N) -> N bxor (N bsr 1).

gray_burn(N, Acc, Limit) when N > Limit ->
    Acc;

gray_burn(N, Acc, Limit) ->
    G = gray(N),
    NewAcc = Acc bxor G,
    gray_burn(N + 1, NewAcc, Limit).
