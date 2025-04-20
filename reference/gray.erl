-module(gray).
-export([main/0]).

main() ->
  io:write(gray_burn(0, 0, 100000)).

gray(N) -> N bxor (N bsr 1).

gray_burn(N, Acc, Limit) when N > Limit -> Acc;
gray_burn(N, Acc, Limit) ->
    gray_burn(N + 1, Acc bxor gray(N), Limit).
